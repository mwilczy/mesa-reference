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
#include "nir/nir_builtin_builder.h"
#include "nir/nir_search_helpers.h"
#include "nir/nir_format_convert.h"
#include "rogue.h"
#include "rogue_nir_fmt_utils.h"
#include "util/macros.h"

/**
 * \file rogue_nir_fmt_utils.c
 *
 * \brief Contains utilities for format transformations.
 */

nir_def *
fmt_pack_scalar(nir_builder *b,
                nir_def *base,
                nir_def *value,
                unsigned dest_bits,
                nir_def *chan,
                const struct util_format_channel_description *chan_desc,
                bool is_texture_pack)
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

   nir_def *packed = NULL;

   /* 32-bit float or pure integer - return as is. */
   if ((is_float || is_pure_int) && bits == 32) {
      packed = value;
   }
   /* < 32-bit pure integer - extract (with sign-extension if signed). */
   else if (is_pure_int && bits < 32) {
      packed = nir_bitfield_insert_imm(b, base, value, offset, bits);
   }
   /* < 32-bit float - float pack. */
   else if (is_float && bits < 32) {
      switch (bits) {
      case 10:
      case 11:
         packed = nir_pack_r11g11b10f_field(b, base, value, chan);
         break;

      case 16:
         packed = nir_pack_half_2x16_field(b, base, value, chan);
         break;

      default:
         unreachable("Unsupported float pack format.");
      }
   }
   /* Normalized - pack. */
   else if (is_norm) {
      switch (bits) {
      case 2:
      case 10:
         packed = is_signed
                     ? nir_pack_snorm_r10g10b10a2_field(b, base, value, chan)
                     : nir_pack_unorm_r10g10b10a2_field(b, base, value, chan);
         break;

      case 5:
      case 6:
         packed = is_signed ? nir_pack_snorm_r5g6b5_field(b, base, value, chan)
                            : nir_pack_unorm_r5g6b5_field(b, base, value, chan);
         break;

      case 8:
         packed = is_signed ? nir_pack_snorm_4x8_field(b, base, value, chan)
                            : nir_pack_unorm_4x8_field(b, base, value, chan);
         break;

      case 16:
         packed = is_signed ? nir_pack_snorm_2x16_field(b, base, value, chan)
                            : nir_pack_unorm_2x16_field(b, base, value, chan);
         break;

      default:
         unreachable("Unsupported normalized pack format.");
      }
   }
   /* Scaled - pack. */
   else if (is_scaled) {
      switch (bits) {
      case 2:
      case 10:
         packed = is_signed
                     ? nir_pack_sscaled_r10g10b10a2_field(b, base, value, chan)
                     : nir_pack_uscaled_r10g10b10a2_field(b, base, value, chan);
         break;

      case 5:
      case 6:
         packed = is_signed
                     ? nir_pack_sscaled_r5g6b5_field(b, base, value, chan)
                     : nir_pack_uscaled_r5g6b5_field(b, base, value, chan);
         break;

      case 8:
         packed = is_signed ? nir_pack_sscaled_4x8_field(b, base, value, chan)
                            : nir_pack_uscaled_4x8_field(b, base, value, chan);
         break;

      case 16:
         packed = is_signed ? nir_pack_sscaled_2x16_field(b, base, value, chan)
                            : nir_pack_uscaled_2x16_field(b, base, value, chan);
         break;

      default:
         unreachable("Unsupported scaled pack format.");
      }
   } else {
      unreachable("Unsupported packing format.");
   }

   if (is_texture_pack)
      packed = nir_ubitfield_extract_imm(b, packed, offset, bits);

   assert(packed);
   return packed;
}

nir_def *
fmt_unpack_scalar(nir_builder *b,
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

   nir_def *unpacked = NULL;

   /* 32-bit float or pure integer - return as is. */
   if ((is_float || is_pure_int) && bits == 32) {
      unpacked = raw_load;
   }
   /* < 32-bit pure integer - extract (with sign-extension if signed). */
   else if (is_pure_int && bits < 32) {
      chan = 0;
      unpacked = is_signed
                    ? nir_ibitfield_extract_imm(b, raw_load, offset, bits)
                    : nir_ubitfield_extract_imm(b, raw_load, offset, bits);
   }
   /* < 32-bit float - float unpack. */
   else if (is_float && bits < 32) {
      switch (bits) {
      case 10:
      case 11:
         unpacked = nir_unpack_r11g11b10f(b, raw_load);
         break;

      case 16:
         unpacked = nir_unpack_half_2x16(b, raw_load);
         break;

      default:
         unreachable("Unsupported float unpack format.");
      }
   }
   /* Normalized - unpack. */
   else if (is_norm) {
      switch (bits) {
      case 2:
      case 10:
         unpacked = is_signed ? nir_unpack_snorm_r10g10b10a2(b, raw_load)
                              : nir_unpack_unorm_r10g10b10a2(b, raw_load);
         break;

      case 5:
      case 6:
         unpacked = is_signed ? nir_unpack_snorm_r5g6b5(b, raw_load)
                              : nir_unpack_unorm_r5g6b5(b, raw_load);
         break;

      case 8:
         unpacked = is_signed ? nir_unpack_snorm_4x8(b, raw_load)
                              : nir_unpack_unorm_4x8(b, raw_load);
         break;

      case 16:
         unpacked = is_signed ? nir_unpack_snorm_2x16(b, raw_load)
                              : nir_unpack_unorm_2x16(b, raw_load);
         break;

      default:
         unreachable("Unsupported normalized unpack format.");
      }
   }
   /* Scaled - unpack. */
   else if (is_scaled) {
      switch (bits) {
      case 2:
      case 10:
         unpacked = is_signed ? nir_unpack_sscaled_r10g10b10a2(b, raw_load)
                              : nir_unpack_uscaled_r10g10b10a2(b, raw_load);
         break;

      case 5:
      case 6:
         unpacked = is_signed ? nir_unpack_sscaled_r5g6b5(b, raw_load)
                              : nir_unpack_uscaled_r5g6b5(b, raw_load);
         break;

      case 8:
         unpacked = is_signed ? nir_unpack_sscaled_4x8(b, raw_load)
                              : nir_unpack_uscaled_4x8(b, raw_load);
         break;

      case 16:
         unpacked = is_signed ? nir_unpack_sscaled_2x16(b, raw_load)
                              : nir_unpack_uscaled_2x16(b, raw_load);
         break;

      default:
         unreachable("Unsupported scaled unpack format.");
      }
   } else {
      unreachable("Unsupported unpacking format.");
   }

   assert(unpacked);
   return nir_channel(b, unpacked, chan);
}

nir_def *
fmt_colorspace_transform_scalar(nir_builder *b,
                                nir_def *value,
                                unsigned bits,
                                enum pipe_swizzle chan,
                                const struct util_format_description *fmt_desc,
                                bool to_colorspace)
{
   enum util_format_colorspace colorspace = fmt_desc->colorspace;

   nir_def *transformed = value;

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

static inline bool fmt_is_float(const struct util_format_description *fmt_desc)
{
   return !get_first_non_void_channel(fmt_desc)->pure_integer;
}

nir_def *get_unspec_chan(nir_builder *b,
                         enum pipe_swizzle chan,
                         unsigned bit_size,
                         const struct util_format_description *fmt_desc)
{
   if (chan == PIPE_SWIZZLE_0)
      return nir_imm_int(b, 0);

   if (chan == PIPE_SWIZZLE_1)
      return fmt_is_float(fmt_desc) ? nir_imm_floatN_t(b, 1.0f, bit_size)
                                    : nir_imm_intN_t(b, 1, bit_size);

   unreachable("Invalid unspecified channel.");
}
