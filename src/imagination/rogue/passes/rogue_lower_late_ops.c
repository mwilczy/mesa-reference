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

#include "rogue.h"
#include "rogue_builder.h"
#include "util/macros.h"

#include <stdbool.h>

/**
 * \file rogue_lower_late_ops.c
 *
 * \brief Contains the rogue_lower_late_ops pass.
 */

static inline bool rogue_lower_alu_instr(rogue_builder *b, rogue_alu_instr *alu)
{
   switch (alu->op) {
   default:
      break;
   }

   return false;
}

static inline bool rogue_lower_END(rogue_builder *b, rogue_ctrl_instr *end)
{
   /* If the instruction before is a non-control instruction, set
    * its end flag instead of emitting a nop.end.
    */
   rogue_instr *prev = rogue_instr_prev(&end->instr);
   if (prev && prev->type != ROGUE_INSTR_TYPE_CTRL) {
      rogue_set_instr_end(prev, true);
      rogue_instr_delete(&end->instr);
      return true;
   }

   rogue_ctrl_instr *nop = rogue_NOP(b);
   rogue_merge_instr_comment(&nop->instr, &end->instr, "end");
   rogue_set_ctrl_op_mod(nop, ROGUE_CTRL_OP_MOD_END);
   rogue_instr_delete(&end->instr);

   return true;
}

static inline bool rogue_lower_ctrl_instr(rogue_builder *b,
                                          rogue_ctrl_instr *ctrl)
{
   switch (ctrl->op) {
   case ROGUE_CTRL_OP_END:
      return rogue_lower_END(b, ctrl);

   default:
      break;
   }

   return false;
}

static inline bool rogue_lower_MOVI(rogue_builder *b, rogue_bitwise_instr *movi)
{
   rogue_bitwise_instr *byp0b =
      rogue_BYP0B(b,
                  rogue_ref_io(ROGUE_IO_FT0),
                  movi->dst[0].ref,
                  rogue_ref_io(ROGUE_IO_S0),
                  rogue_ref_val(rogue_ref_get_imm(&movi->src[0].ref)->imm.u32));

   rogue_merge_instr_comment(&byp0b->instr, &movi->instr, "movi");
   rogue_instr_delete(&movi->instr);

   return true;
}

static inline bool rogue_lower_bitwise_instr(rogue_builder *b,
                                             rogue_bitwise_instr *bitwise)
{
   switch (bitwise->op) {
   case ROGUE_BITWISE_OP_MOVI:
      return rogue_lower_MOVI(b, bitwise);

   default:
      break;
   }

   return false;
}

PUBLIC
bool rogue_lower_late_ops(rogue_shader *shader)
{
   if (shader->is_grouped)
      return false;

   bool progress = false;

   rogue_builder b;
   rogue_builder_init(&b, shader);

   rogue_foreach_instr_in_shader_safe (instr, shader) {
      /* Skip real ops. */
      if (rogue_instr_phase(instr) != ROGUE_INSTR_PHASE_INVALID)
         continue;

      b.cursor = rogue_cursor_before_instr(instr);
      switch (instr->type) {
      case ROGUE_INSTR_TYPE_ALU:
         progress |= rogue_lower_alu_instr(&b, rogue_instr_as_alu(instr));
         break;

      case ROGUE_INSTR_TYPE_CTRL:
         progress |= rogue_lower_ctrl_instr(&b, rogue_instr_as_ctrl(instr));
         break;

      case ROGUE_INSTR_TYPE_BITWISE:
         progress |=
            rogue_lower_bitwise_instr(&b, rogue_instr_as_bitwise(instr));
         break;

      default:
         continue;
      }
   }

   return progress;
}
