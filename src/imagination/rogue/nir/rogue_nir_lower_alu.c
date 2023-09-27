/*
 * Copyright © 2023 Imagination Technologies Ltd.
 *
 * based in part on nir_to_dxil which is:
 * Copyright © Microsoft Corporation
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

#include "nir.h"
#include "nir_builder.h"
#include "nir_builtin_builder.h"
#include "rogue.h"

/**
 * \file rogue_nir_lower_alu.c
 *
 * \brief Contains various alu-lowering passes.
 */

/* TODO: support pack as well. */
static bool is_alu_conversion(const nir_instr *instr, UNUSED const void *_data)
{
   return instr->type == nir_instr_type_alu &&
          nir_op_infos[nir_instr_as_alu(instr)->op].is_conversion;
}

static nir_def *
lower_alu_conversion(nir_builder *b, nir_instr *instr, UNUSED void *_data)
{
   nir_alu_instr *alu = nir_instr_as_alu(instr);
   nir_def *src = nir_ssa_for_alu_src(b, alu, 0);
   nir_alu_type src_type = nir_op_infos[alu->op].input_types[0] | src->bit_size;
   nir_alu_type dst_type = nir_op_infos[alu->op].output_type;

   nir_rounding_mode rounding_mode = nir_rounding_mode_undef;
   bool sat = false;

   switch (alu->op) {
   case nir_op_f2f16_rtne:
      rounding_mode = nir_rounding_mode_rtne;
      break;

   case nir_op_f2f16_rtz:
      rounding_mode = nir_rounding_mode_rtz;
      break;

   default:
      break;
   }

   return nir_convert_alu_types(b,
                                alu->def.bit_size,
                                src,
                                .src_type = src_type,
                                .dest_type = dst_type,
                                .rounding_mode = rounding_mode,
                                .saturate = sat);
}

bool rogue_nir_lower_alu_conversion_to_intrinsic(nir_shader *shader)
{
   return nir_shader_lower_instructions(shader,
                                        is_alu_conversion,
                                        lower_alu_conversion,
                                        NULL);
}

static bool is_fquantize2f16(const nir_instr *instr, const void *data)
{
   if (instr->type != nir_instr_type_alu)
      return false;

   nir_alu_instr *alu = nir_instr_as_alu(instr);
   return alu->op == nir_op_fquantize2f16;
}

static nir_def *
lower_fquantize2f16(struct nir_builder *b, nir_instr *instr, UNUSED void *data)
{
   nir_alu_instr *alu = nir_instr_as_alu(instr);
   nir_def *src = nir_ssa_for_alu_src(b, alu, 0);

   nir_def *f_inf = nir_imm_float(b, INFINITY);
   nir_def *zero = nir_imm_int(b, 0);

   nir_def *max_half = nir_imm_int(b, 0x477fe000);
   nir_def *min_half = nir_imm_int(b, 0x38800000);

   nir_def *abs_src = nir_fabs(b, src);

   /* TODO: use conversion alu ops instead of this intrinsic */
   nir_def *res = nir_convert_alu_types(b,
                                16,
                                src,
                                .src_type = nir_type_float32,
                                .dest_type = nir_type_float16,
                                .rounding_mode = nir_rounding_mode_undef,
                                .saturate = false);

   res = nir_convert_alu_types(b,
                                32,
                                res,
                                .src_type = nir_type_float16,
                                .dest_type = nir_type_float32,
                                .rounding_mode = nir_rounding_mode_undef,
                                .saturate = false);

   res = nir_bcsel(b, nir_fisnan(b, src), src, res);
   res = nir_bcsel(b, nir_flt(b, max_half, abs_src), nir_copysign_img(b, f_inf, src), res);
   res = nir_bcsel(b, nir_flt(b, abs_src, min_half), nir_copysign_img(b, zero, src), res);

   return res;
}

PUBLIC
bool rogue_nir_lower_fquantize2f16(nir_shader *shader)
{
   return nir_shader_lower_instructions(shader,
                                        is_fquantize2f16,
                                        lower_fquantize2f16,
                                        NULL);
}

static bool is_fround_even(const nir_instr *instr, const void *data)
{
   if (instr->type != nir_instr_type_alu)
      return false;

   nir_alu_instr *alu = nir_instr_as_alu(instr);
   return alu->op == nir_op_fround_even;
}

static nir_def *
lower_fround_even(struct nir_builder *b, nir_instr *instr, void *data)
{
   nir_alu_instr *alu = nir_instr_as_alu(instr);
   nir_def *src = nir_ssa_for_src(b,
                                  alu->src[0].src,
                                  nir_src_num_components(alu->src[0].src));

   nir_def *abs_src = nir_fabs(b, src);
   nir_def *ffloor_temp = nir_ffloor(b, abs_src);
   nir_def *ffract_temp = nir_ffract(b, abs_src);

   nir_def *ceil_temp = nir_fadd_imm(b, ffloor_temp, 1.0f);
   nir_def *even_temp = nir_fmul_imm(b, ffloor_temp, 0.5f);

   nir_def *even_ffract_temp = nir_ffract(b, even_temp);

   nir_def *ishalf_temp = nir_feq_imm(b, ffract_temp, 0.5f);
   ffract_temp = nir_bcsel(b, ishalf_temp, even_ffract_temp, ffract_temp);

   nir_def *lesshalf_temp = nir_flt_imm(b, ffract_temp, 0.5f);
   nir_def *result_temp = nir_bcsel(b, lesshalf_temp, ffloor_temp, ceil_temp);

   nir_def *res = nir_copysign_img(b, result_temp, src);

   return res;
}

PUBLIC
bool rogue_nir_lower_fround_even(nir_shader *shader)
{
   return nir_shader_lower_instructions(shader,
                                        is_fround_even,
                                        lower_fround_even,
                                        NULL);
}
