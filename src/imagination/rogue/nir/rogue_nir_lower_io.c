/*
 * Copyright © 2022 Imagination Technologies Ltd.
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
#include "nir/nir_search_helpers.h"
#include "rogue.h"
#include "util/macros.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * \file rogue_nir_lower_io.c
 *
 * \brief Contains the rogue_nir_lower_io pass.
 */

/*
 * Scalarize/convert load_workgroup_id to our custom intrinsics.
 * Unused components will get DCEd later.
 */
static bool lower_load_workgroup_id(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->def.num_components == 3);
   assert(intr->def.bit_size == 32);

   b->cursor = nir_before_instr(&intr->instr);

   nir_def *wgid_x = nir_load_workgroup_id_x_img(b);
   nir_def *wgid_y = nir_load_workgroup_id_y_img(b);
   nir_def *wgid_z = nir_load_workgroup_id_z_img(b);

   nir_def_rewrite_uses(&intr->def, nir_vec3(b, wgid_x, wgid_y, wgid_z));
   nir_instr_remove(&intr->instr);

   return true;
}

/*
 * Scalarize/convert load_num_workgroups to loads.
 * Unused components will get DCEd later.
 */
static bool lower_load_num_workgroups(nir_builder *b, nir_intrinsic_instr *intr)
{
   assert(intr->def.num_components == 3);
   assert(intr->def.bit_size == 32);

   b->cursor = nir_before_instr(&intr->instr);

   /* This will be handled in rogue_nir_to_rogue, load the base address from
    * shareds. */
   nir_def *num_wgs_base_addr = nir_load_num_workgroups_base_addr_img(b);

   nir_def *num_wgs[3];
   unsigned load_align = intr->def.bit_size / 8;
   unsigned num_components = intr->def.num_components;

   /* Load each component. */
   for (unsigned c = 0; c < num_components; ++c) {
      unsigned offset = c * (intr->def.bit_size / 8);
      num_wgs[c] =
         nir_load_global_constant(b,
                                  nir_iadd_imm(b, num_wgs_base_addr, offset),
                                  load_align,
                                  1,
                                  intr->def.bit_size);
   }

   nir_def_rewrite_uses(&intr->def, nir_vec(b, num_wgs, num_components));
   nir_instr_remove(&intr->instr);

   return true;
}

static bool lower_load_push_constant(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   /* This will be handled in rogue_nir_to_rogue, load the base address from
    * shareds. */
   nir_def *push_consts_base_addr = nir_load_push_consts_base_addr_img(b);

   /* Calculate offset for the push constant. */
   nir_def *offset = intr->src[0].ssa;
   unsigned load_align = intr->def.bit_size / 8;

   /* Load the push constant. */
   nir_def *push_const = nir_load_global_constant(
      b,
      nir_iadd(b, push_consts_base_addr, nir_u2u64(b, offset)),
      load_align,
      intr->def.num_components,
      intr->def.bit_size);

   nir_def_rewrite_uses(&intr->def, push_const);
   nir_instr_remove(&intr->instr);

   return true;
}

/* Simply check for duplicate discards and remove one. */
static bool lower_discard(nir_builder *b, nir_intrinsic_instr *intr)
{
   nir_instr *instr_next = nir_instr_next(&intr->instr);
   if (!instr_next)
      return false;

   if (instr_next->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr_next = nir_instr_as_intrinsic(instr_next);
   if (intr_next->intrinsic != nir_intrinsic_discard)
      return false;

   nir_instr_remove(&intr->instr);

   return true;
}

/* Just return zero for now. */
static bool lower_load_base_instance(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);

   nir_def_rewrite_uses(&intr->def, nir_imm_int(b, 0));
   nir_instr_remove(&intr->instr);

   return true;
}

/* Convert {load,store}_shared intrinsics to their img-specific versions.
 * These utilise the coefficient registers, and so require DWORD offsets.
 *
 * TODO: Coefficient register spilling support.
 */
static bool
lower_load_store_shared(nir_builder *b, nir_intrinsic_instr *intr, bool store)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(nir_intrinsic_base(intr) == 0);

   nir_src offset_src = intr->src[!!store];
   assert(nir_src_num_components(offset_src) == 1);

   /* To DWORD offset/addressing. */
   nir_def *offset = nir_ushr_imm(b, offset_src.ssa, 2);

   if (store) {
      assert(nir_intrinsic_write_mask(intr) == 1);
      nir_def *value = intr->src[0].ssa;

      nir_store_shared_img(b, value, offset);
   } else {
      assert(intr->def.num_components == 1);

      nir_def *load_shared_img = nir_load_shared_img(b, offset);
      nir_def_rewrite_uses(&intr->def, load_shared_img);
   }

   nir_instr_remove(&intr->instr);

   return true;
}

/* Convert shared_atomic{,_swap} intrinsics to their img-specific versions.
 * These utilise the coefficient registers, and so require DWORD offsets.
 *
 * TODO: Coefficient register spilling support.
 */
static bool
lower_shared_atomic(nir_builder *b, nir_intrinsic_instr *intr, bool swap)
{
   b->cursor = nir_before_instr(&intr->instr);

   assert(nir_intrinsic_base(intr) == 0);

   nir_def *offset = intr->src[0].ssa;

   /* To DWORD offset/addressing. */
   offset = nir_ushr_imm(b, offset, 2);

   assert(nir_src_num_components(intr->src[1]) == 1);
   nir_def *value = intr->src[1].ssa;

   nir_def *value_swap = swap ? intr->src[2].ssa : NULL;

   nir_def *shared_atomic;
   if (swap) {
      shared_atomic =
         nir_shared_atomic_swap_img(b,
                                    offset,
                                    value,
                                    value_swap,
                                    .atomic_op = nir_intrinsic_atomic_op(intr));
   } else {
      shared_atomic =
         nir_shared_atomic_img(b,
                               offset,
                               value,
                               .atomic_op = nir_intrinsic_atomic_op(intr));
   }

   nir_def_rewrite_uses(&intr->def, shared_atomic);

   nir_instr_remove(&intr->instr);

   return true;
}

static bool lower_load_preamble(nir_builder *b, nir_intrinsic_instr *intr)
{
   unsigned bit_size = intr->def.bit_size;
   assert(bit_size <= 32);

   unsigned num_components = intr->def.num_components;
   if (num_components == 1)
      return false;

   b->cursor = nir_before_instr(&intr->instr);

   nir_def *scalar_comps[NIR_MAX_VEC_COMPONENTS];
   unsigned base = nir_intrinsic_base(intr);

   /* Load each component. */
   for (unsigned c = 0; c < num_components; ++c)
      scalar_comps[c] = nir_load_preamble(b, 1, bit_size, .base = base + c);

   nir_def_rewrite_uses(&intr->def, nir_vec(b, scalar_comps, num_components));
   nir_instr_remove(&intr->instr);

   return true;
}

static bool lower_store_preamble(nir_builder *b, nir_intrinsic_instr *intr)
{
   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   assert(bit_size <= 32);

   unsigned num_components = nir_src_num_components(intr->src[0]);
   if (num_components == 1)
      return false;

   b->cursor = nir_before_instr(&intr->instr);

   unsigned base = nir_intrinsic_base(intr);

   /* Store each component. */
   for (unsigned c = 0; c < num_components; ++c)
      nir_store_preamble(b,
                         nir_channel(b, intr->src[0].ssa, c),
                         .base = base + c);

   nir_instr_remove(&intr->instr);

   return true;
}

static bool lower_intrinsic(nir_builder *b,
                            nir_intrinsic_instr *instr,
                            rogue_build_ctx *ctx,
                            bool late)
{
   if (!late) {
      switch (instr->intrinsic) {
      case nir_intrinsic_load_preamble:
         return lower_load_preamble(b, instr);

      case nir_intrinsic_store_preamble:
         return lower_store_preamble(b, instr);

      default:
         break;
      }
   } else {
      switch (instr->intrinsic) {
      case nir_intrinsic_load_workgroup_id:
         return lower_load_workgroup_id(b, instr);

      case nir_intrinsic_load_num_workgroups:
         return lower_load_num_workgroups(b, instr);

      case nir_intrinsic_load_push_constant:
         return lower_load_push_constant(b, instr);

      case nir_intrinsic_discard:
         return lower_discard(b, instr);

      case nir_intrinsic_load_base_instance:
         return lower_load_base_instance(b, instr);

      case nir_intrinsic_load_shared:
         return lower_load_store_shared(b, instr, false);

      case nir_intrinsic_store_shared:
         return lower_load_store_shared(b, instr, true);

      case nir_intrinsic_shared_atomic:
         return lower_shared_atomic(b, instr, false);

      case nir_intrinsic_shared_atomic_swap:
         return lower_shared_atomic(b, instr, true);

      default:
         break;
      }
   }

   return false;
}

static bool lower_impl(nir_function_impl *impl, rogue_build_ctx *ctx, bool late)
{
   bool progress = false;
   nir_builder b = nir_builder_create(impl);

   nir_foreach_block (block, impl) {
      nir_foreach_instr_safe (instr, block) {
         b.cursor = nir_before_instr(instr);
         switch (instr->type) {
         case nir_instr_type_intrinsic:
            progress |=
               lower_intrinsic(&b, nir_instr_as_intrinsic(instr), ctx, late);
            break;

         default:
            break;
         }
      }
   }

   if (progress)
      nir_metadata_preserve(impl, nir_metadata_none);
   else
      nir_metadata_preserve(impl, nir_metadata_all);

   return progress;
}

PUBLIC
bool rogue_nir_lower_io(nir_shader *shader, rogue_build_ctx *ctx, bool late)
{
   bool progress = false;

   nir_foreach_function (function, shader) {
      if (function->impl)
         progress |= lower_impl(function->impl, ctx, late);
   }

   if (progress)
      nir_opt_dce(shader);

   return progress;
}
