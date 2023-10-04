/*
 * Copyright © 2023 Imagination Technologies Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "nir.h"
#include "nir_builder.h"
#include "rogue.h"
#include "rogue_nir_fmt_utils.h"

/**
 * \file rogue_nir_lower_input_attachments.c
 *
 * \brief Contains the rogue_nir_lower_input_attachments pass.
 */

/* TODO: rename to rogue_nir_lower_input_attachments_to_...
 * since this only handles off-chip ones.
 */

struct state {
   nir_shader *shader;
   struct rogue_fs_build_data *fs_data;
};

static nir_def *lower_input_attachment(nir_builder *b, nir_instr *instr, void *cb_data)
{
   struct state *state = (struct state*)cb_data;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   nir_binding image_binding = nir_chase_binding(intr->src[0]);
   assert(image_binding.success);

   nir_variable *image_var = nir_get_binding_variable(state->shader, image_binding);
   assert(image_var);
   assert(image_var->data.mode & nir_var_image);

   unsigned i = image_var->data.index; /* TODO: check this */
   unsigned l = state->fs_data->inputs[i].on_chip_rt + FRAG_RESULT_DATA0;

   return nir_load_output(b,
                      intr->def.num_components,
                      intr->def.bit_size,
                      nir_imm_int(b, 0),
                      .base = 0,
                      .component = 0,
                      .dest_type = nir_intrinsic_dest_type(intr),
                      .io_semantics.location = l,
                      .io_semantics.num_slots = 1/*,
                      .io_semantics.fb_fetch_output = true*/);
}

static bool is_input_attachment(const nir_instr *instr, UNUSED const void *cb_data)
{
   struct state *state = (struct state*)cb_data;

   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_image_deref_load)
      return false;

   enum glsl_sampler_dim image_dim = nir_intrinsic_image_dim(intr);

   /* TODO: support GLSL_SAMPLER_DIM_SUBPASS_MS */
   /* assert(image_dim != GLSL_SAMPLER_DIM_SUBPASS_MS); */
   if (image_dim != GLSL_SAMPLER_DIM_SUBPASS)
      return false;

   nir_binding image_binding = nir_chase_binding(intr->src[0]);
   assert(image_binding.success);

   nir_variable *image_var = nir_get_binding_variable(state->shader, image_binding);
   assert(image_var);
   assert(image_var->data.mode & nir_var_image);

   unsigned i = image_var->data.index; /* TODO: check this */
   assert(i < state->fs_data->num_inputs);
   enum pvr_renderpass_hwsetup_input_access type = state->fs_data->inputs[i].type;
   if (type == PVR_RENDERPASS_HWSETUP_INPUT_ACCESS_OFFCHIP)
      return false;

   /* TODO: PVR_RENDERPASS_HWSETUP_INPUT_ACCESS_ONCHIP_ZREPLICATE */
   /* assert(type == PVR_RENDERPASS_HWSETUP_INPUT_ACCESS_ONCHIP); */

   bool is_stencil = util_format_has_stencil(util_format_description(state->fs_data->inputs[i].format)) && (glsl_get_sampler_result_type(image_var->type) != GLSL_TYPE_FLOAT);
   if (is_stencil)
      return false;

   return true;
}

bool
rogue_nir_lower_input_attachments(nir_shader *shader, rogue_build_ctx *ctx)
{
   struct state state = {
      .shader = shader,
      .fs_data = &ctx->stage_data.fs,
   };

   return nir_shader_lower_instructions(shader, is_input_attachment, lower_input_attachment, &state);
}
