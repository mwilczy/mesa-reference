/*
 * Copyright © 2023 Imagination Technologies Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "nir/nir.h"
#include "nir/nir_builder.h"
#include "rogue.h"
#include "util/macros.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * \file rogue_nir_compute.c
 *
 * \brief Contains compute-specific NIR passes.
 */

#define ROGUE_INST_CHK_FUNC_NAME "__rogue_inst_chk_func__"

/* Inserts an instance check for compute shaders. */
PUBLIC
bool rogue_nir_compute_instance_check(nir_shader *shader)
{
   /* Only apply to compute shaders. */
   if (shader->info.stage != MESA_SHADER_COMPUTE)
      return false;

   /* Check we haven't already done this. */
   nir_foreach_function (function, shader) {
      if (!function->name || !strcmp(function->name, ROGUE_INST_CHK_FUNC_NAME))
         return false;
   }

   /* Get original entrypoint. */
   nir_function_impl *orig_entrypoint_impl = nir_shader_get_entrypoint(shader);
   nir_function *orig_entrypoint = orig_entrypoint_impl->function;

   /* Create a function for the instance check which will serve as the new
    * entrypoint.
    */
   nir_function *inst_chk_func =
      nir_function_create(shader, ROGUE_INST_CHK_FUNC_NAME);
   nir_function_impl *inst_chk_func_impl =
      nir_function_impl_create(inst_chk_func);

   inst_chk_func->is_entrypoint = true;
   orig_entrypoint->is_entrypoint = false;

   nir_builder b = nir_builder_create(inst_chk_func_impl);
   b.cursor = nir_after_cf_list(&inst_chk_func_impl->body);

   /* If the current instance index is greater than the total workgroup size,
    * we don't execute.
    */

   nir_def *local_size = nir_load_workgroup_size(&b);
   nir_def *size_x = nir_channel(&b, local_size, 0);
   nir_def *size_y = nir_channel(&b, local_size, 1);
   nir_def *size_z = nir_channel(&b, local_size, 2);

   nir_def *flat_size = nir_imul(&b, nir_imul(&b, size_x, size_y), size_z);
   nir_def *flat_id = nir_load_local_invocation_index(&b);
   nir_def *cond_inst_valid = nir_ilt32(&b, flat_id, flat_size);

   nir_if *nif = nir_push_if(&b, cond_inst_valid);
   nif->control = nir_selection_control_dont_flatten;

   nir_block *if_then_block = nir_if_first_then_block(nif);

   /* Call the original entrypoint (will get inlined). */
   nir_call_instr *ncall = nir_call_instr_create(shader, orig_entrypoint);
   nir_instr_insert_after_block(if_then_block, &ncall->instr);

   nir_pop_if(&b, nif);
   nir_jump(&b, nir_jump_return);

   return true;
}

/*
 * Scalarize/convert load_workgroup_id to our custom intrinsics.
 * Unused components will get DCEd later.
 */
static nir_def *lower_load_workgroup_id(nir_builder *b,
                                        nir_intrinsic_instr *intr,
                                        UNUSED void *cb_data)
{
   assert(intr->def.num_components == 3);
   assert(intr->def.bit_size == 32);

   nir_def *wgid_x = nir_load_workgroup_id_x_img(b);
   nir_def *wgid_y = nir_load_workgroup_id_y_img(b);
   nir_def *wgid_z = nir_load_workgroup_id_z_img(b);

   return nir_vec3(b, wgid_x, wgid_y, wgid_z);
}

/*
 * Scalarize/convert load_num_workgroups to loads.
 * Unused components will get DCEd later.
 */
static nir_def *lower_load_num_workgroups(nir_builder *b,
                                          nir_intrinsic_instr *intr,
                                          UNUSED void *cb_data)
{
   unsigned num_components = intr->def.num_components;
   unsigned bit_size = intr->def.bit_size;
   unsigned load_align = bit_size / 8;

   assert(num_components == 3);
   assert(bit_size == 32);

   /* This will be handled in rogue_nir_to_rogue;
    * load the base address from shareds.
    */
   nir_def *num_wgs_base_addr = nir_load_num_workgroups_base_addr_img(b);

   nir_def *num_wgs[3];
   /* Load each component. */
   for (unsigned c = 0; c < num_components; ++c) {
      unsigned offset = c * load_align;
      num_wgs[c] =
         nir_load_global_constant(b,
                                  nir_iadd_imm(b, num_wgs_base_addr, offset),
                                  load_align,
                                  1,
                                  bit_size);
   }

   return nir_vec(b, num_wgs, num_components);
}

static bool is_compute_intrinsic(const nir_instr *instr,
                                 UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   return (intr->intrinsic == nir_intrinsic_load_workgroup_id) ||
          (intr->intrinsic == nir_intrinsic_load_num_workgroups);
}

static nir_def *
lower_compute_intrinsic(nir_builder *b, nir_instr *instr, void *cb_data)
{
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   switch (intr->intrinsic) {
   case nir_intrinsic_load_workgroup_id:
      return lower_load_workgroup_id(b, intr, cb_data);

   case nir_intrinsic_load_num_workgroups:
      return lower_load_num_workgroups(b, intr, cb_data);

   default:
      break;
   }

   unreachable();
}

PUBLIC
bool rogue_nir_lower_compute_intrinsics(nir_shader *shader)
{
   return nir_shader_lower_instructions(shader,
                                        is_compute_intrinsic,
                                        lower_compute_intrinsic,
                                        NULL);
}
