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
 * \file rogue_nir_lower_syncops.c
 *
 * \brief Contains passes that lower synchronisation ops.
 */

struct lower_atomics_options {
   unsigned atomic_op_mask;
   nir_variable_mode atomic_op_modes;
};

/**
 * \brief Emits a (optionally mutex-locked) loop that will execute an operation
 * once per each active instance.
 *
 * \param[in] b The NIR builder context.
 * \param[in] locked Whether to mutex-lock the loop (across slots).
 * \param[in] result_num_components The number of components for the result.
 * \param[in] result_bit_size The bit size for the result.
 * \param[in] per_inst_op A pointer to the dummy per-instance op to be replaced.
 * \param[in] loop_result A pointer to the phi node containing the loop result.
 *
 * %inst_num = @load_instance_num_img()
 * %iter_start = 0
 * %iter_check = ROGUE_MAX_INSTANCES_PER_TASK
 * %iter_incr = 1
 * %undef = undefined
 *
 * loop {
 *    %phi_iter_ssa = phi %iter_start, %next_iter
 *    %phi_result_out_ssa = phi %undef, %phi_result_ssa
 *
 *    if (%phi_iter_ssa >= %iter_check)
 *       break;
 *
 *    if (%phi_iter_ssa == %inst_num)
 *       %per_inst_op = ...
 *
 *    %phi_result_ssa = phi %per_inst_op, %phi_result_out_ssa
 *    %next_iter = iadd %phi_iter_ssa, %iter_incr
 * }
 */
static void rogue_per_instance_loop(nir_builder *b,
                                    bool locked,
                                    unsigned result_num_components,
                                    unsigned result_bit_size,
                                    nir_def **per_inst_op,
                                    nir_def **loop_result)
{
   nir_shader *shader = b->shader;

   /* Set up the loop. */
   nir_def *inst_num = nir_load_instance_num_img(b);
   nir_def *iter_start = nir_imm_int(b, 0);
   nir_def *iter_check = nir_imm_int(b, ROGUE_MAX_INSTANCES_PER_TASK);
   nir_def *iter_incr = nir_imm_int(b, 1);
   nir_def *undef = nir_undef(b, result_num_components, result_bit_size);

   if (locked) {
      nir_mutex_img(b,
                    .mutex_id_img = ROGUE_MUTEX_ID_ATOMIC_EMU,
                    .mutex_op_img = ROGUE_MUTEX_OP_LOCK);
   }

   /* Create phi for loop iterator. */
   nir_phi_instr *phi_iter = nir_phi_instr_create(shader);
   nir_def_init(&phi_iter->instr,
                &phi_iter->def,
                iter_start->num_components,
                iter_start->bit_size);
   nir_def *phi_iter_ssa = &phi_iter->def;

   /* Create phis for loop result. */
   nir_phi_instr *phi_result_out = nir_phi_instr_create(shader);
   nir_def_init(&phi_result_out->instr,
                &phi_result_out->def,
                result_num_components,
                result_bit_size);
   nir_def *phi_result_out_ssa = &phi_result_out->def;

   nir_phi_instr *phi_result = nir_phi_instr_create(shader);
   nir_def_init(&phi_result->instr,
                &phi_result->def,
                result_num_components,
                result_bit_size);
   nir_def *phi_result_ssa = &phi_result->def;

   /* Dummy per-instance op to be replaced. */
   nir_def *dummy_op;

   /* Create loop, don't allow it to be unrolled. */
   nir_loop *per_inst_loop = nir_push_loop(b);
   nir_block *pre_loop =
      nir_block_cf_tree_prev(nir_loop_first_block(per_inst_loop));
   {
      nir_phi_instr_add_src(phi_iter, pre_loop, iter_start);

      nir_phi_instr_add_src(phi_result_out, pre_loop, undef);

      /* Loop test. */
      nir_def *loop_cond = nir_uge(b, phi_iter_ssa, iter_check);
      nir_if *loop_break_if = nir_push_if(b, loop_cond);
      {
         nir_jump(b, nir_jump_break);
      }
      loop_break_if->control = nir_selection_control_dont_flatten;
      nir_pop_if(b, loop_break_if);

      /* Per-instance loop body. */
      nir_def *per_inst_cond = nir_ieq(b, phi_iter_ssa, inst_num);
      nir_if *inst_num_if = nir_push_if(b, per_inst_cond);
      {
         /* To be replaced by the caller of this function. */
         nir_undef_instr *undef = nir_undef_instr_create(shader,
                                                         result_num_components,
                                                         result_bit_size);
         nir_builder_instr_insert(b, &undef->instr);
         dummy_op = &undef->def;

         nir_phi_instr_add_src(phi_result,
                               nir_if_last_then_block(inst_num_if),
                               dummy_op);
      }
      inst_num_if->control = nir_selection_control_dont_flatten;
      nir_pop_if(b, inst_num_if);

      nir_phi_instr_add_src(phi_result_out,
                            nir_cursor_current_block(b->cursor),
                            phi_result_ssa);

      nir_phi_instr_add_src(phi_result,
                            nir_if_last_else_block(inst_num_if),
                            phi_result_out_ssa);
      nir_builder_instr_insert(b, &phi_result->instr);

      /* Loop increment src for phi_iter_ssa. */
      nir_def *next_iter = nir_iadd(b, phi_iter_ssa, iter_incr);
      nir_phi_instr_add_src(phi_iter,
                            nir_cursor_current_block(b->cursor),
                            next_iter);
   }
   per_inst_loop->control = nir_loop_control_dont_unroll;
   nir_pop_loop(b, per_inst_loop);

   if (locked) {
      nir_mutex_img(b,
                    .mutex_id_img = ROGUE_MUTEX_ID_ATOMIC_EMU,
                    .mutex_op_img = ROGUE_MUTEX_OP_RELEASE);
   }

   b->cursor = nir_before_cf_list(&per_inst_loop->body);
   nir_builder_instr_insert(b, &phi_iter->instr);
   nir_builder_instr_insert(b, &phi_result_out->instr);

   *per_inst_op = dummy_op;
   *loop_result = phi_result_out_ssa;
}

static nir_def *emit_atomic_op(nir_builder *b,
                               nir_atomic_op op,
                               nir_def *pre_val,
                               nir_def *value,
                               nir_def *value_swap)
{
   ASSERTED nir_alu_type type = nir_atomic_op_type(op);

   switch (op) {
   case nir_atomic_op_iadd:
      assert(type == nir_type_uint);
      return nir_iadd(b, pre_val, value);

   case nir_atomic_op_imin:
      assert(type == nir_type_int);
      return nir_imin(b, pre_val, value);

   case nir_atomic_op_umin:
      assert(type == nir_type_uint);
      return nir_umin(b, pre_val, value);

   case nir_atomic_op_imax:
      assert(type == nir_type_int);
      return nir_imax(b, pre_val, value);

   case nir_atomic_op_umax:
      assert(type == nir_type_uint);
      return nir_umax(b, pre_val, value);

   case nir_atomic_op_iand:
      assert(type == nir_type_uint);
      return nir_iand(b, pre_val, value);

   case nir_atomic_op_ior:
      assert(type == nir_type_uint);
      return nir_ior(b, pre_val, value);

   case nir_atomic_op_ixor:
      assert(type == nir_type_uint);
      return nir_ixor(b, pre_val, value);

   case nir_atomic_op_xchg:
      assert(type == nir_type_uint);
      return nir_mov(b, value);

   case nir_atomic_op_fadd:
      assert(type == nir_type_float);
      return nir_fadd(b, pre_val, value);

   case nir_atomic_op_fmin:
      assert(type == nir_type_float);
      return nir_fmin(b, pre_val, value);

   case nir_atomic_op_fmax:
      assert(type == nir_type_float);
      return nir_fmax(b, pre_val, value);

   case nir_atomic_op_cmpxchg:
      assert(type == nir_type_uint);
      assert(value_swap);
      return nir_bcsel(b, nir_ieq(b, pre_val, value), value_swap, pre_val);

   case nir_atomic_op_fcmpxchg:
      assert(type == nir_type_float);
      assert(value_swap);
      return nir_bcsel(b, nir_feq(b, pre_val, value), value_swap, pre_val);

   default:
      break;
   }

   unreachable("Unsupported atomic op.");
}

static nir_def *
lower_atomic(nir_builder *b, nir_instr *instr, UNUSED void *_data)
{
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   unsigned num_components = intr->def.num_components;
   assert(num_components == 1);

   ASSERTED unsigned bit_size = intr->def.bit_size;
   assert(bit_size == 32); /* TODO: support other bit sizes. */

   nir_atomic_op op = nir_intrinsic_atomic_op(intr);

   bool is_shared = intr->intrinsic == nir_intrinsic_shared_atomic_img ||
                    intr->intrinsic == nir_intrinsic_shared_atomic_swap_img;
   bool is_swap = intr->intrinsic == nir_intrinsic_global_atomic_swap ||
                  intr->intrinsic == nir_intrinsic_shared_atomic_swap_img;

   nir_def *offset = intr->src[0].ssa;
   nir_def *value = intr->src[1].ssa;
   nir_def *value_swap = NULL;
   if (is_swap)
      value_swap = intr->src[2].ssa;

   /* Create per-instance locked loop to emulate the atomic op. */
   nir_def *atom_result;
   nir_def *per_inst_op;
   rogue_per_instance_loop(b,
                           true,
                           num_components,
                           bit_size,
                           &per_inst_op,
                           &atom_result);
   b->cursor = nir_after_instr(per_inst_op->parent_instr);

   /* Get the pre-op value. */
   nir_def *pre_val;
   if (is_shared)
      pre_val = nir_load_shared_img(b, offset);
   else
      pre_val =
         nir_load_global(b, offset, bit_size / 8, num_components, bit_size);

   /* Emit the atomic op. */
   nir_def *post_val = emit_atomic_op(b, op, pre_val, value, value_swap);

   /* Replace the dummy loop op with the atomic op. */
   nir_def_rewrite_uses(per_inst_op, pre_val);

   if (is_shared) {
      nir_store_shared_img(b, post_val, offset);
   } else {
      nir_store_global(b,
                       offset,
                       bit_size / 8,
                       post_val,
                       BITFIELD_MASK(num_components));
   }

   return atom_result;
}

static bool is_lowerable_atomic(const nir_instr *instr, const void *data)
{
   const struct lower_atomics_options *options = data;

   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   switch (intr->intrinsic) {
   case nir_intrinsic_shared_atomic:
   case nir_intrinsic_shared_atomic_swap:
      unreachable(
         "shared_atomic(_swap) should've been lowered before this pass.");
      break;

   case nir_intrinsic_global_atomic:
   case nir_intrinsic_global_atomic_swap:
      if (!(options->atomic_op_modes & nir_var_mem_global))
         return false;

      break;

   case nir_intrinsic_shared_atomic_img:
   case nir_intrinsic_shared_atomic_swap_img:
      if (!(options->atomic_op_modes & nir_var_mem_shared))
         return false;

      break;

   default:
      return false;
   }

   if (BITFIELD_BIT(nir_intrinsic_atomic_op(intr)) & options->atomic_op_mask)
      return true;

   return false;
}

static void
rogue_setup_lower_atomics_options(struct lower_atomics_options *options)
{
   if (ROGUE_DEBUG(ATOMIC_EMU)) {
      options->atomic_op_mask = ~0;
      options->atomic_op_modes = ~0;
      return;
   }

   /* For global memory, cmpxchg and float ops aren't natively supported.
    * All shared memory ops can be performed in a single instruction group,
    * and so don't need to be lowered.
    */
   options->atomic_op_modes = nir_var_mem_global;
   options->atomic_op_mask =
      BITFIELD_BIT(nir_atomic_op_fadd) | BITFIELD_BIT(nir_atomic_op_fmin) |
      BITFIELD_BIT(nir_atomic_op_fmax) | BITFIELD_BIT(nir_atomic_op_fcmpxchg);

   /* TODO: Don't lower this on cores where it's natively supported. */
   options->atomic_op_mask |= BITFIELD_BIT(nir_atomic_op_cmpxchg);
}

PUBLIC
bool rogue_nir_lower_atomics(nir_shader *shader)
{
   struct lower_atomics_options options;
   rogue_setup_lower_atomics_options(&options);

   return nir_shader_lower_instructions(shader,
                                        is_lowerable_atomic,
                                        lower_atomic,
                                        &options);
}

static bool is_barrier(const nir_instr *instr, UNUSED const void *data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   return intr->intrinsic == nir_intrinsic_barrier;
}

static nir_def *
lower_barrier(struct nir_builder *b, nir_instr *instr, void *data)
{
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   mesa_scope exec_scope = nir_intrinsic_execution_scope(intr);
   if (exec_scope == SCOPE_NONE) {
      nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_BRANCH);
      return NIR_LOWER_INSTR_PROGRESS_REPLACE;
   }
#if 0
   mesa_scope mem_scope = nir_intrinsic_memory_scope(intr);
   nir_memory_semantics mem_smnt = nir_intrinsic_memory_semantics(intr);
   nir_variable_mode mem_modes = nir_intrinsic_memory_modes(intr);
#endif

   unsigned wg_size = b->shader->info.workgroup_size[0] * b->shader->info.workgroup_size[1] * b->shader->info.workgroup_size[2];
   if (wg_size <= RGX_INSTANCES_PER_SLOT)
      return NIR_LOWER_INSTR_PROGRESS_REPLACE;

   unsigned num_slots = DIV_ROUND_UP(wg_size, RGX_INSTANCES_PER_SLOT);
   nir_def *slot_count = nir_imm_int(b, num_slots);

   nir_mutex_img(b,
               .mutex_id_img = ROGUE_MUTEX_ID_BARRIER,
               .mutex_op_img = ROGUE_MUTEX_OP_LOCK);

   nir_def *inst_num = nir_load_instance_num_img(b);
   nir_def *is_inst_0 = nir_ieq(b, inst_num, nir_imm_int(b, 0));

   nir_if *nif_is_inst_0 = nir_push_if(b, is_inst_0);
   {
      nir_barrier_counter_set_img(b, nir_imm_int(b, 1), .flags = true);
   }
   nir_pop_if(b, nif_is_inst_0);

   /* nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL_BARRIER_COUNTER); */
   nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL);
   nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_BRANCH);

   nir_def *all_slots_done = nir_barrier_counter_cmp_img(b, slot_count);
   nir_if *nif_all_slots_done = nir_push_if(b, all_slots_done);
   {
      inst_num = nir_load_instance_num_img(b);
      is_inst_0 = nir_ieq(b, inst_num, nir_imm_int(b, 0));
      nir_if *nif_is_inst_0 = nir_push_if(b, is_inst_0);
      {
         nir_barrier_counter_set_img(b, nir_imm_int(b, 0));
      }
      nir_pop_if(b, nif_is_inst_0);

      /* nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL_BARRIER_COUNTER); */
      nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL);
      nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_BRANCH);

      nir_mutex_img(b,
                  .mutex_id_img = ROGUE_MUTEX_ID_BARRIER,
                  .mutex_op_img = ROGUE_MUTEX_OP_RELEASE_WAKEUP);
   }
   nir_push_else(b, nif_all_slots_done);
   {
      nir_loop *sleep_wake_loop = nir_push_loop(b);
      {
         nir_mutex_img(b, .mutex_id_img = ROGUE_MUTEX_ID_BARRIER, .mutex_op_img = ROGUE_MUTEX_OP_RELEASE_SLEEP);
         nir_mutex_img(b, .mutex_id_img = ROGUE_MUTEX_ID_BARRIER, .mutex_op_img = ROGUE_MUTEX_OP_LOCK);

         /* nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL_BARRIER_COUNTER); */
         nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL);
         nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_BRANCH);
         /* nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL); */

         nir_nop(b);
         nir_nop(b);
         nir_nop(b);
         nir_nop(b);
         nir_nop(b);

         /* Loop test. */
         nir_def *loop_cond = nir_barrier_counter_cmp_img(b, nir_imm_int(b, 0));
         nir_if *loop_break_if = nir_push_if(b, loop_cond);
         {
            nir_jump(b, nir_jump_break);
         }
         nir_pop_if(b, loop_break_if);
      }
      nir_pop_loop(b, sleep_wake_loop);

      /* nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL_BARRIER_COUNTER); */
      nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL);
      nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_BRANCH);

      nir_mutex_img(b,
            .mutex_id_img = ROGUE_MUTEX_ID_BARRIER,
            .mutex_op_img = ROGUE_MUTEX_OP_RELEASE);
   }
   nir_pop_if(b, nif_all_slots_done);

   /* nir_fence_img(b, .fence_op_img = ROGUE_FENCE_OP_LOCAL_BARRIER_COUNTER); */

   b->shader->info.uses_control_barrier = true;
   /* b->shader->info.uses_memory_barrier = true; */

   return NIR_LOWER_INSTR_PROGRESS_REPLACE;
}

PUBLIC
bool rogue_nir_lower_barriers(nir_shader *shader)
{
   return nir_shader_lower_instructions(shader, is_barrier, lower_barrier, NULL);
}
