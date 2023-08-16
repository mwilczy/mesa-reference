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
#include "rogue.h"
#include "util/macros.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * \file rogue_nir_opt_fold_packs.c
 *
 * \brief Contains the rogue_nir_opt_fold_packs pass.
 */

static nir_def *fold_pack(nir_builder *b, nir_op op, nir_def *src)
{
   switch (op) {
   case nir_op_pack_unorm_2x16_field:
      return nir_pack_unorm_2x16(b, src);

   case nir_op_pack_snorm_2x16_field:
      return nir_pack_snorm_2x16(b, src);

   case nir_op_pack_uscaled_2x16_field:
      return nir_pack_uscaled_2x16(b, src);

   case nir_op_pack_sscaled_2x16_field:
      return nir_pack_sscaled_2x16(b, src);

   case nir_op_pack_r11g11b10f_field:
      return nir_pack_r11g11b10f(b, src);

   case nir_op_pack_unorm_4x8_field:
      return nir_pack_unorm_4x8(b, src);

   case nir_op_pack_snorm_4x8_field:
      return nir_pack_snorm_4x8(b, src);

   case nir_op_pack_uscaled_4x8_field:
      return nir_pack_uscaled_4x8(b, src);

   case nir_op_pack_sscaled_4x8_field:
      return nir_pack_sscaled_4x8(b, src);

   case nir_op_pack_unorm_r10g10b10a2_field:
      return nir_pack_unorm_r10g10b10a2(b, src);

   case nir_op_pack_snorm_r10g10b10a2_field:
      return nir_pack_snorm_r10g10b10a2(b, src);

   case nir_op_pack_uscaled_r10g10b10a2_field:
      return nir_pack_uscaled_r10g10b10a2(b, src);

   case nir_op_pack_sscaled_r10g10b10a2_field:
      return nir_pack_sscaled_r10g10b10a2(b, src);

   case nir_op_pack_unorm_r5g6b5_field:
      return nir_pack_unorm_r5g6b5(b, src);

   case nir_op_pack_snorm_r5g6b5_field:
      return nir_pack_snorm_r5g6b5(b, src);

   case nir_op_pack_uscaled_r5g6b5_field:
      return nir_pack_uscaled_r5g6b5(b, src);

   case nir_op_pack_sscaled_r5g6b5_field:
      return nir_pack_sscaled_r5g6b5(b, src);

   default:
      break;
   }

   unreachable("Unsupported op.");
}

static unsigned num_pack_comps(nir_op op)
{
   switch (op) {
   case nir_op_pack_unorm_2x16_field:
   case nir_op_pack_snorm_2x16_field:
   case nir_op_pack_uscaled_2x16_field:
   case nir_op_pack_sscaled_2x16_field:
      return 2;

   case nir_op_pack_unorm_r5g6b5_field:
   case nir_op_pack_snorm_r5g6b5_field:
   case nir_op_pack_uscaled_r5g6b5_field:
   case nir_op_pack_sscaled_r5g6b5_field:
   case nir_op_pack_r11g11b10f_field:
      return 3;

   case nir_op_pack_unorm_4x8_field:
   case nir_op_pack_snorm_4x8_field:
   case nir_op_pack_uscaled_4x8_field:
   case nir_op_pack_sscaled_4x8_field:
   case nir_op_pack_unorm_r10g10b10a2_field:
   case nir_op_pack_snorm_r10g10b10a2_field:
   case nir_op_pack_uscaled_r10g10b10a2_field:
   case nir_op_pack_sscaled_r10g10b10a2_field:
      return 4;

   default:
      break;
   }

   unreachable("Unsupported op.");
}

static bool fold_field_packs(nir_builder *b, nir_alu_instr *alu)
{
   unsigned num_comps = num_pack_comps(alu->op);

   unsigned num_packs = 0;
   nir_def *pack_src[4] = { 0 };

   /* Walk back each base and store the value + element. */
   nir_alu_instr *pack = alu;
   bool used_only_once = true;
   do {
      /* The final pack can be used more than once,
       * but don't bother with this if the rest are.
       */
      if (!used_only_once)
         return false;

      /* Store the pack source value in element order. */
      nir_def *comp = nir_ssa_for_alu_src(b, pack, 1);
      unsigned elem = nir_src_as_uint(pack->src[2].src);

      assert(pack_src[elem] == NULL);
      pack_src[elem] = comp;

      nir_def *split_pack_dest = &pack->def;
      used_only_once = list_is_singular(&split_pack_dest->uses);
      ++num_packs;

      pack = nir_src_as_alu_instr(pack->src[0].src);
   } while (pack && pack->op == alu->op);

   /* One or more packs may have been optimised out; don't bother. */
   if (num_packs != num_comps)
      return false;

   /* Emit a folded pack. */
   nir_def *pack_srcs = nir_vec(b, pack_src, num_comps);
   nir_def *folded_pack = fold_pack(b, alu->op, pack_srcs);

   nir_def_rewrite_uses(&alu->def, folded_pack);
   nir_instr_remove(&alu->instr);

   return true;
}

static unsigned instr_is_field_pack(const nir_instr *instr)
{
   if (instr->type != nir_instr_type_alu)
      return false;

   nir_alu_instr *alu = nir_instr_as_alu(instr);
   switch (alu->op) {
   case nir_op_pack_r11g11b10f_field:
   /* case nir_op_pack_snorm_2x16_field: */
   /* case nir_op_pack_snorm_4x8_field: */
   case nir_op_pack_snorm_r10g10b10a2_field:
   case nir_op_pack_snorm_r5g6b5_field:
   /* case nir_op_pack_sscaled_2x16_field: */
   /* case nir_op_pack_sscaled_4x8_field: */
   case nir_op_pack_sscaled_r10g10b10a2_field:
   case nir_op_pack_sscaled_r5g6b5_field:
   /* case nir_op_pack_unorm_2x16_field: */
   /* case nir_op_pack_unorm_4x8_field: */
   case nir_op_pack_unorm_r10g10b10a2_field:
   case nir_op_pack_unorm_r5g6b5_field:
   /* case nir_op_pack_uscaled_2x16_field: */
   /* case nir_op_pack_uscaled_4x8_field: */
   case nir_op_pack_uscaled_r10g10b10a2_field:
   case nir_op_pack_uscaled_r5g6b5_field:
      return true;

   default:
      break;
   }

   return false;
}

static bool lower_impl(nir_function_impl *impl)
{
   bool progress = false;
   nir_builder b = nir_builder_create(impl);

   nir_foreach_block_reverse(block, impl)
   {
      nir_foreach_instr_reverse_safe(instr, block)
      {
         if (!instr_is_field_pack(instr))
            continue;

         b.cursor = nir_after_instr(instr);
         progress |= fold_field_packs(&b, nir_instr_as_alu(instr));
      }
   }

   if (progress)
      nir_metadata_preserve(impl, nir_metadata_none);
   else
      nir_metadata_preserve(impl, nir_metadata_all);

   return progress;
}

PUBLIC
bool rogue_nir_opt_fold_packs(nir_shader *shader)
{
   bool progress = false;

   nir_foreach_function (function, shader) {
      if (function->impl)
         progress |= lower_impl(function->impl);
   }

   return progress;
}
