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
#include "nir/nir_builtin_builder.h"
#include "nir/nir_search_helpers.h"
#include "nir/nir_format_convert.h"
#include "rogue.h"
#include "util/macros.h"
#include "vk_graphics_state.h"
#include "vulkan/vk_format.h"

/**
 * \file rogue_nir_pfo.c
 *
 * \brief Contains PFO (per-fragment operation) passes.
 */

static nir_def *
nir_fsat_to_format(nir_builder *b, nir_def *x, enum pipe_format format)
{
   if (util_format_is_unorm(format))
      return nir_fsat(b, x);
   else if (util_format_is_snorm(format))
      return nir_fsat_signed(b, x);
   else
      return x;
}

static inline nir_def *do_pack(nir_builder *b,
                               enum pvr_pbe_accum_format pbe,
                               nir_def *chans,
                               enum pipe_format format)
{
   const unsigned bits_2101010[] = { 10, 10, 10, 2 };
   const unsigned bits_8888[] = { 8, 8, 8, 8 };
   const unsigned bits_1616[] = { 16, 16 };

   switch (pbe) {
   case PVR_PBE_ACCUM_FORMAT_U8:
      return nir_pack_unorm_4x8(b, chans);

   case PVR_PBE_ACCUM_FORMAT_S8:
      return nir_pack_snorm_4x8(b, chans);

   case PVR_PBE_ACCUM_FORMAT_U16:
      return nir_pack_unorm_2x16(b, chans);

   case PVR_PBE_ACCUM_FORMAT_S16:
      return nir_pack_snorm_2x16(b, chans);

   case PVR_PBE_ACCUM_FORMAT_F16:
      return nir_pack_half_2x16(b, nir_fsat_to_format(b, chans, format));

   case PVR_PBE_ACCUM_FORMAT_F32:
   case PVR_PBE_ACCUM_FORMAT_SINT32:
   case PVR_PBE_ACCUM_FORMAT_UINT32:
      return chans;

   case PVR_PBE_ACCUM_FORMAT_U1010102:
      return nir_format_pack_uint(b, chans, bits_2101010, chans->num_components);

   case PVR_PBE_ACCUM_FORMAT_UINT8:
   case PVR_PBE_ACCUM_FORMAT_SINT8:
      return nir_format_pack_uint(b, chans, bits_8888, chans->num_components);

   case PVR_PBE_ACCUM_FORMAT_UINT16:
   case PVR_PBE_ACCUM_FORMAT_SINT16:
      return nir_format_pack_uint(b, chans, bits_1616, chans->num_components);

   default:
      break;
   }

   unreachable("Unsupported pbe_accum_format.");
   return NULL;
}

static inline nir_def *do_unpack(nir_builder *b,
                                 enum pvr_pbe_accum_format pbe,
                                 nir_def *pck,
                                 unsigned num_components)
{
   const unsigned bits_2101010[] = { 10, 10, 10, 2 };
   const unsigned bits_8888[] = { 8, 8, 8, 8 };
   const unsigned bits_1616[] = { 16, 16 };

   switch (pbe) {
   case PVR_PBE_ACCUM_FORMAT_U8:
      return nir_unpack_unorm_4x8(b, pck);

   case PVR_PBE_ACCUM_FORMAT_S8:
      return nir_unpack_snorm_4x8(b, pck);

   case PVR_PBE_ACCUM_FORMAT_U16:
      return nir_unpack_unorm_2x16(b, pck);

   case PVR_PBE_ACCUM_FORMAT_S16:
      return nir_unpack_snorm_2x16(b, pck);

   case PVR_PBE_ACCUM_FORMAT_F16:
      return nir_unpack_half_2x16(b, pck);

   case PVR_PBE_ACCUM_FORMAT_F32:
   case PVR_PBE_ACCUM_FORMAT_SINT32:
   case PVR_PBE_ACCUM_FORMAT_UINT32:
      return pck;

   case PVR_PBE_ACCUM_FORMAT_U1010102:
      return nir_format_unpack_uint(b, pck, bits_2101010, num_components);

   case PVR_PBE_ACCUM_FORMAT_UINT8:
      return nir_format_unpack_uint(b, pck, bits_8888, num_components);

   case PVR_PBE_ACCUM_FORMAT_SINT8:
      return nir_format_unpack_int(b, pck, bits_8888, num_components, true);

   case PVR_PBE_ACCUM_FORMAT_UINT16:
      return nir_format_unpack_uint(b, pck, bits_1616, num_components);

   case PVR_PBE_ACCUM_FORMAT_SINT16:
      return nir_format_unpack_int(b, pck, bits_1616, num_components, true);

   default:
      break;
   }

   unreachable("Unsupported pbe_accum_format.");
   return NULL;
}

static inline const struct glsl_type *
pbe_accum_to_glsl_type(enum pvr_pbe_accum_format pbe)
{
   switch (pbe) {
   case PVR_PBE_ACCUM_FORMAT_U8:
   case PVR_PBE_ACCUM_FORMAT_UINT8:
   case PVR_PBE_ACCUM_FORMAT_U16:
   case PVR_PBE_ACCUM_FORMAT_UINT16:
   case PVR_PBE_ACCUM_FORMAT_U1010102:
   case PVR_PBE_ACCUM_FORMAT_UINT32:
      return glsl_uintN_t_type(32);

   case PVR_PBE_ACCUM_FORMAT_S8:
   case PVR_PBE_ACCUM_FORMAT_SINT8:
   case PVR_PBE_ACCUM_FORMAT_S16:
   case PVR_PBE_ACCUM_FORMAT_SINT16:
   case PVR_PBE_ACCUM_FORMAT_SINT32:
      return glsl_intN_t_type(32);

   case PVR_PBE_ACCUM_FORMAT_F16:
   case PVR_PBE_ACCUM_FORMAT_F32:
      return glsl_floatN_t_type(32);

   default:
      break;
   }

   unreachable("Unsupported pbe_accum_format.");
   return NULL;
}

static nir_def *lower_output_io(nir_builder *b, nir_instr *instr, void *cb_data)
{
   struct rogue_fs_build_data *fs_data = (struct rogue_fs_build_data *)cb_data;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   bool is_store = intr->intrinsic == nir_intrinsic_store_output;

   nir_io_semantics sem = nir_intrinsic_io_semantics(intr);
   nir_variable *output_var =
      nir_find_variable_with_location(b->shader,
                                      nir_var_shader_out,
                                      sem.location);
   unsigned location = sem.location - FRAG_RESULT_DATA0;
   unsigned mrt_idx = location + nir_src_as_uint(intr->src[!!is_store]);
   assert(mrt_idx < fs_data->num_outputs);
   const struct usc_mrt_resource *mrt_resource =
      fs_data->outputs[mrt_idx].mrt_resource;

   assert(mrt_resource->type == USC_MRT_RESOURCE_TYPE_OUTPUT_REG);
   assert(mrt_resource->reg.offset == 0);

   enum pvr_pbe_accum_format pbe = fs_data->outputs[mrt_idx].accum_format;
   enum pipe_format format = fs_data->outputs[mrt_idx].format;
   unsigned components = util_format_get_nr_components(format);

   unsigned pbe_dwords =
      DIV_ROUND_UP(mrt_resource->intermediate_size, sizeof(uint32_t));

   const struct glsl_type *glsl_type = pbe_accum_to_glsl_type(pbe);
   nir_alu_type nir_type = nir_get_nir_type_for_glsl_type(glsl_type);

   /* Rewrite the old store and perform the pack/conversion if needed. */

   b->cursor = nir_before_instr(instr);

   unsigned size = DIV_ROUND_UP(components, pbe_dwords);

   if (is_store) {
      nir_src *output_src = &intr->src[0];

      for (unsigned out = 0; out < pbe_dwords; ++out) {
         nir_component_mask_t chan_mask =
            nir_component_mask(MIN2(size, components - out * size));

         nir_def *chans =
            nir_channels(b, output_src->ssa, chan_mask << (out * size));
         chans = nir_resize_vector(b, chans, size);

         /* TODO: Make the pack optional. */
         nir_def *pack = do_pack(b, pbe, chans, format);

         nir_store_output(b,
                          pack,
                          nir_imm_int(b, 0),
                          .base = mrt_resource->reg.output_reg + out,
                          .src_type = nir_type,
                          .write_mask = 1,
                          .io_semantics = sem);
      }

      /* Update the variable and dereference types. */
      if (output_var)
         output_var->type = glsl_type;

      return NIR_LOWER_INSTR_PROGRESS_REPLACE;
   }

   bool is_float = nir_alu_type_get_base_type(nir_type) == nir_type_float;

   int num_unpack_comp = 0;
   nir_def *unpacks[4];

   for (unsigned in = 0; in < pbe_dwords; ++in) {
      nir_def *load = nir_load_output(b,
                                      1,
                                      32,
                                      nir_imm_int(b, 0),
                                      .base = mrt_resource->reg.output_reg + in,
                                      .dest_type = nir_type,
                                      .io_semantics = sem);

      nir_def *upck = do_unpack(b, pbe, load, size);
      for (unsigned c = 0; c < MIN2(size, components - in * size); c++)
         unpacks[num_unpack_comp++] = nir_channel(b, upck, c);
   }

   /* Missing .gba comps */
   for (; num_unpack_comp < 3; num_unpack_comp++)
      unpacks[num_unpack_comp] = nir_imm_int(b, 0);
   if (num_unpack_comp == 3) {
      unpacks[num_unpack_comp++] = is_float ? nir_imm_float(b, 1.0)
                                            : nir_imm_int(b, 0);
   }

   assert(num_unpack_comp == 4);
   return nir_vec(b, unpacks, num_unpack_comp);
}

static bool is_output_io(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_store_output &&
       intr->intrinsic != nir_intrinsic_load_output)
      return false;

   nir_io_semantics sem = nir_intrinsic_io_semantics(intr);
   assert(sem.dual_source_blend_index == 0 && "Should have been lowered");
   return (sem.location >= FRAG_RESULT_DATA0);
}

static nir_def *lower_load_front_face(nir_builder *b,
                                      nir_intrinsic_instr *intr,
                                      const struct rogue_fs_build_data *fs_data)
{
   nir_def *front_face;

   /* TODO: Handle cores that have a front face special register.  */
   const bool sr_is_front = false;
   if (sr_is_front) {
      front_face = nir_i2b(b, nir_instr_def(&intr->instr));
   } else {
      nir_def *face_orient = nir_load_face_orientation_img(b);
      nir_def *clockwise = nir_imm_int(b, ROGUE_FACE_ORIENT_CLOCKWISE);
      front_face = nir_ieq(b, face_orient, clockwise);
   }

   switch (fs_data->swap_front_face) {
   /* No change. */
   case SWAP_FRONT_FACE_FALSE:
      return front_face;

   /* Invert. */
   case SWAP_FRONT_FACE_TRUE:
      return nir_inot(b, front_face);

   /* Force true. */
   case SWAP_FRONT_FACE_FORCE:
      return nir_imm_true(b);

   default:
      break;
   }

   unreachable("Unsupported swap_front_face enum value.");
}

static nir_def *
lower_frag_intrinsic(nir_builder *b, nir_instr *instr, void *cb_data)
{
   const struct rogue_fs_build_data *fs_data =
      (const struct rogue_fs_build_data *)cb_data;
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   switch (intr->intrinsic) {
   case nir_intrinsic_load_front_face:
      return lower_load_front_face(b, intr, fs_data);

   default:
      break;
   }

   unreachable("Unsupported fs intrinsic.");
}

static bool is_frag_intrinsic(const nir_instr *instr,
                              UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   switch (intr->intrinsic) {
   case nir_intrinsic_load_front_face:
      return true;

   default:
      break;
   }

   return false;
}

PUBLIC
bool rogue_nir_pfo(nir_shader *shader, rogue_build_ctx *ctx)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   bool progress = false;

   /* Fragment result data I/O. */
   progress |= nir_shader_lower_instructions(shader,
                                             is_output_io,
                                             lower_output_io,
                                             &ctx->stage_data.fs);

   /* TODO: Depth/sample mask outputs. */

   /* Frag intrinsics. */
   progress |= nir_shader_lower_instructions(shader,
                                             is_frag_intrinsic,
                                             lower_frag_intrinsic,
                                             &ctx->stage_data.fs);

   return progress;
}
