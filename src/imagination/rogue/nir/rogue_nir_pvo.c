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
#include "nir/nir_search_helpers.h"
#include "nir/nir_format_convert.h"
#include "rogue.h"
#include "util/macros.h"
#include "vulkan/vk_format.h"

/**
 * \file rogue_nir_pvo.c
 *
 * \brief Contains per-vertex operation passes.
 */

static nir_def *
do_unpack(nir_builder *b,
          nir_def *raw_load,
          unsigned dest_bits,
          enum pipe_swizzle chan,
          const struct util_format_channel_description *chan_desc)
{
   enum util_format_type type = chan_desc->type;
   unsigned offset = chan_desc->shift % ROGUE_REG_SIZE_BITS;
   unsigned bits = chan_desc->size;
   bool is_float = type == UTIL_FORMAT_TYPE_FLOAT;
   bool is_int = (type == UTIL_FORMAT_TYPE_UNSIGNED) ||
                 (type == UTIL_FORMAT_TYPE_SIGNED);
   bool is_pure_int = chan_desc->pure_integer;
   bool is_norm = chan_desc->normalized;
   bool is_scaled = is_int && !is_pure_int && !is_norm;
   bool is_signed = type != UTIL_FORMAT_TYPE_UNSIGNED;

   /* TODO: Also support: */
   assert(type != UTIL_FORMAT_TYPE_FIXED);
   assert(dest_bits == 32);

   nir_def *unpacked = raw_load;

   /* 32-bit float or pure integer - return as is. */
   if ((is_float || is_pure_int) && bits == 32) {
   }
   /* < 32-bit pure integer - extract (with sign-extension if signed). */
   else if (is_pure_int && bits < 32) {
      return is_signed ? nir_ibitfield_extract(b,
                                               unpacked,
                                               nir_imm_int(b, offset),
                                               nir_imm_int(b, bits))
                       : nir_ubitfield_extract(b,
                                               unpacked,
                                               nir_imm_int(b, offset),
                                               nir_imm_int(b, bits));
   }
   /* < 32-bit float - float unpack. */
   else if (is_float && bits < 32) {
      switch (bits) {
      case 10:
      case 11:
         unpacked = nir_unpack_r11g11b10f(b, unpacked);
         break;

      case 16:
         unpacked = nir_unpack_half_2x16(b, unpacked);
         break;

      default:
         unreachable("Unsupported float unpack format.");
      }
   }
   /* Normalised - unpack. */
   else if (is_norm) {
      switch (bits) {
      case 2:
      case 10:
         unpacked = is_signed ? nir_unpack_snorm_r10g10b10a2(b, unpacked)
                              : nir_unpack_unorm_r10g10b10a2(b, unpacked);
         break;

      case 8:
         unpacked = is_signed ? nir_unpack_snorm_4x8(b, unpacked)
                              : nir_unpack_unorm_4x8(b, unpacked);
         break;

      case 16:
         unpacked = is_signed ? nir_unpack_snorm_2x16(b, unpacked)
                              : nir_unpack_unorm_2x16(b, unpacked);
         break;

      default:
         unreachable("Unsupported normalised unpack format.");
      }
   }
   /* Scaled - unpack. */
   else if (is_scaled) {
      switch (bits) {
      case 2:
      case 10:
         unpacked = is_signed ? nir_unpack_sscaled_r10g10b10a2(b, unpacked)
                              : nir_unpack_uscaled_r10g10b10a2(b, unpacked);
         break;

      case 8:
         unpacked = is_signed ? nir_unpack_sscaled_4x8(b, unpacked)
                              : nir_unpack_uscaled_4x8(b, unpacked);
         break;

      case 16:
         unpacked = is_signed ? nir_unpack_sscaled_2x16(b, unpacked)
                              : nir_unpack_uscaled_2x16(b, unpacked);
         break;

      default:
         unreachable("Unsupported scaled unpack format.");
      }
   } else {
      unreachable("Unsupported vertex input format.");
   }

   return nir_channel(b, unpacked, chan);
}

static nir_def *
do_colorspace_transform(nir_builder *b,
                        nir_def *unpacked,
                        unsigned bits,
                        enum pipe_swizzle chan,
                        const struct util_format_description *fmt_desc)
{
   enum util_format_colorspace colorspace = fmt_desc->colorspace;

   nir_def *transformed = unpacked;

   /* TODO: Support other colorspaces. */
   if (colorspace == UTIL_FORMAT_COLORSPACE_RGB)
      return transformed;

   unreachable("Unsupported colorspace.");
}

static inline const struct util_format_channel_description *
get_first_non_void_channel(const struct util_format_description *fmt_desc)
{
   for (unsigned c = 0; c < ARRAY_SIZE(fmt_desc->channel); ++c)
      if (fmt_desc->channel[c].type != UTIL_FORMAT_TYPE_VOID)
         return &fmt_desc->channel[c];

   unreachable();
}

static bool chan_is_float(unsigned chan,
                          const struct util_format_description *fmt_desc)
{
   const struct util_format_channel_description *chan_desc =
      (fmt_desc->channel[chan].type == UTIL_FORMAT_TYPE_VOID)
         ? get_first_non_void_channel(fmt_desc)
         : &fmt_desc->channel[chan];

   return !chan_desc->pure_integer;
}

static nir_def *get_unspec_chan(nir_builder *b,
                                enum pipe_swizzle chan,
                                unsigned bit_size,
                                const struct util_format_description *fmt_desc)
{
   if (chan == PIPE_SWIZZLE_0)
      return nir_imm_int(b, 0);

   if (chan == PIPE_SWIZZLE_1)
      return chan_is_float(chan, fmt_desc) ? nir_imm_floatN_t(b, 1.0f, bit_size)
                                           : nir_imm_intN_t(b, 1, bit_size);

   unreachable("Invalid unspecified channel.");
}

static nir_def *lower_vert_in(nir_builder *b, nir_instr *instr, void *cb_data)
{
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   const struct rogue_vs_build_data *vs_data =
      (const struct rogue_vs_build_data *)cb_data;
   const rogue_vertex_inputs *inputs = &vs_data->inputs;

   b->cursor = nir_before_instr(instr);

   unsigned bit_size = intr->def.bit_size;

   /* TODO: vector loads */
   unsigned load_components = intr->def.num_components;
   assert(load_components == 1);

   nir_io_semantics io_sem = nir_intrinsic_io_semantics(intr);
   assert(nir_src_as_uint(intr->src[0]) == 0 &&
          "Needs nir_io_add_const_offset_to_base");

   unsigned l = io_sem.location - VERT_ATTRIB_GENERIC0;
   unsigned base_vtxin_reg = inputs->base_vtxin_reg[l];
   assert(inputs->defined[l] && base_vtxin_reg != ROGUE_REG_UNUSED);

   const struct util_format_description *fmt_desc = &inputs->format_descs[l];
   assert(fmt_desc);

   assert(!fmt_desc->is_mixed); /* TODO: leaving out mixed types for now. */

   unsigned load_component = nir_intrinsic_component(intr);
   enum pipe_swizzle chan = fmt_desc->swizzle[load_component];

   /* Unspecified components. */
   if (chan > PIPE_SWIZZLE_W)
      return get_unspec_chan(b, chan, bit_size, fmt_desc);

   unsigned fmt_chans = fmt_desc->nr_channels;
   assert(chan < fmt_chans);

   const struct util_format_channel_description *chan_desc =
      &fmt_desc->channel[chan];

   /* Vertex input register offset that contains this channel. */
   unsigned vtxin_offset = chan_desc->shift / ROGUE_REG_SIZE_BITS;

   /* Vertex input register containing the channel/component. */
   unsigned chan_vtxin_reg_first = base_vtxin_reg + vtxin_offset;

   ASSERTED unsigned chan_vtxin_reg_last =
      base_vtxin_reg +
      ((chan_desc->shift + chan_desc->size - 1) / ROGUE_REG_SIZE_BITS);

   /* Make sure the channel doesn't span multiple registers. */
   assert(chan_vtxin_reg_first == chan_vtxin_reg_last);

   /* Load vertex input register. */
   nir_def *raw_load = nir_load_input(b,
                                      load_components,
                                      ROGUE_REG_SIZE_BITS,
                                      nir_imm_int(b, 0),
                                      .base = chan_vtxin_reg_first,
                                      .component = 0,
                                      .dest_type = nir_type_invalid,
                                      .io_semantics = io_sem);

   /* Number of vertex input registers that will contain the format. */
   unsigned num_vtxin_regs = PVR_BITS_TO_DW(fmt_desc->block.bits);

   /* Adjust the channel component.
    * E.g. for [R16G16][B16A16]:
    * var.r => [0].x
    * var.g => [0].y
    * var.b => [1].x
    * var.a => [1].y
    */
   assert(fmt_chans >= num_vtxin_regs);
   assert(!(fmt_chans % num_vtxin_regs));
   chan %= (fmt_chans / num_vtxin_regs);

   nir_def *result = do_unpack(b, raw_load, bit_size, chan, chan_desc);
   result = do_colorspace_transform(b, result, bit_size, chan, fmt_desc);
   return result;
}

static bool is_vert_in(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_load_input)
      return false;

   nir_io_semantics sem = nir_intrinsic_io_semantics(intr);
   assert(sem.location >= VERT_ATTRIB_GENERIC0 &&
          sem.location <= VERT_ATTRIB_GENERIC15);

   return true;
}

PUBLIC
bool rogue_nir_pvo(nir_shader *shader, rogue_build_ctx *ctx)
{
   assert(shader->info.stage == MESA_SHADER_VERTEX);

   return nir_shader_lower_instructions(shader,
                                        is_vert_in,
                                        lower_vert_in,
                                        &ctx->stage_data.vs);
}
