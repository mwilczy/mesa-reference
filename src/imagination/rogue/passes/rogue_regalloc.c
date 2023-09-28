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

#include "rogue.h"
#include "rogue_builder.h"
#include "util/macros.h"
#include "util/ralloc.h"
#include "util/register_allocate.h"
#include "util/sparse_array.h"
#include "util/u_qsort.h"

#include <stdbool.h>
#include <stdlib.h>

/**
 * \file rogue_regalloc.c
 *
 * \brief Contains the rogue_regalloc pass.
 */

#define ROGUE_RA_CLASS_INFO_NODE_SIZE 64

/* TODO: Internal register support for high register pressure regs. */
/* TODO: vtxin register support. */

typedef struct rogue_ra_class_info {
   unsigned stride;
   unsigned class_index;
   enum rogue_reg_class hw_class;
} rogue_ra_class_info;

typedef struct rogue_live_range {
   unsigned start;
   unsigned end;
} rogue_live_range;

static void rogue_regarray_liveness(rogue_regarray *regarray,
                                    rogue_live_range *live_range)
{
   assert(list_is_singular(&regarray->writes) ||
          list_is_empty(&regarray->writes));
   if (!list_is_empty(&regarray->writes)) {
      rogue_regarray_write *write =
         list_first_entry(&regarray->writes, rogue_regarray_write, link);
      live_range->start = MIN2(live_range->start, write->instr->index);
      live_range->end = MAX2(live_range->end, live_range->start);
   }

   rogue_foreach_regarray_use (use, regarray) {
      live_range->end = MAX2(live_range->end, use->instr->index);
   }
}

static void rogue_reg_liveness(rogue_reg *reg, rogue_live_range *live_range)
{
   assert(list_is_singular(&reg->writes) || list_is_empty(&reg->writes));

   if (!list_is_empty(&reg->writes)) {
      rogue_reg_write *write =
         list_first_entry(&reg->writes, rogue_reg_write, link);
      live_range->start = MIN2(live_range->start, write->instr->index);
      live_range->end = MAX2(live_range->end, live_range->start);
   }

   rogue_foreach_reg_use (use, reg) {
      live_range->end = MAX2(live_range->end, use->instr->index);
   }
}

static int regarray_cmp(const void *lhs, const void *rhs, UNUSED void *arg)
{
   const rogue_regarray *const *l = lhs;
   const rogue_regarray *const *r = rhs;

   /* Signs swapped for sorting largest->smallest. */
   if ((*l)->size > (*r)->size)
      return -1;
   else if ((*l)->size < (*r)->size)
      return 1;

   return 0;
}

static inline void rogue_ra_setup_class(struct ra_regs *ra_regs,
                                        struct util_sparse_array *ra_class_info,
                                        unsigned stride,
                                        enum rogue_reg_class hw_class,
                                        unsigned num_hw_prealloced,
                                        unsigned num_hw)
{
   struct ra_class *ra_class = ra_alloc_contig_reg_class(ra_regs, stride);
   unsigned class_index = ra_class_index(ra_class);

   rogue_ra_class_info *class_info =
      util_sparse_array_get(ra_class_info, stride);
   class_info->stride = stride;
   class_info->class_index = class_index;
   class_info->hw_class = hw_class;

   for (unsigned t = num_hw_prealloced; t < num_hw - (stride - 1); ++t)
      ra_class_add_reg(ra_class, t);
}

/* TODO: Track successors/predecessors and do this properly when
 * implementing full regalloc.
 */
static inline void
rogue_extend_loop_reg_lifetimes(rogue_shader *shader,
                                rogue_live_range *ssa_live_range,
                                unsigned num_ssa_regs)
{
   rogue_live_range *loop_live_range =
      rzalloc_array_size(NULL, sizeof(*loop_live_range), shader->loops);

   unsigned l = 0;
   rogue_foreach_instr_in_shader (instr, shader) {
      if (instr->type != ROGUE_INSTR_TYPE_CTRL)
         continue;

      const rogue_ctrl_instr *ctrl = rogue_instr_as_ctrl(instr);
      if (!ctrl->loop_start)
         continue;

      loop_live_range[l].start = instr->index;
      loop_live_range[l].end = ctrl->loop_link->index;

      ++l;
   }

   /* If an ssa reg starts before a loop and stops being used in the middle of a loop,
    * mark it as used until the end of the loop. */
   for (unsigned u = 0; u < num_ssa_regs; ++u) {
      for (l = 0; l < shader->loops; ++l) {
         if ((ssa_live_range[u].start <= loop_live_range[l].start) &&
             (ssa_live_range[u].end >= loop_live_range[l].start) &&
             (ssa_live_range[u].end <= loop_live_range[l].end)) {
            ssa_live_range[u].end = loop_live_range[l].end;
         }
      }
   }

   ralloc_free(loop_live_range);
}

static void insert_spill(rogue_builder *b, unsigned spill_index, bool load)
{
   rogue_shader *shader = b->shader;

   rogue_instr *instr = &rogue_BYP0B(b,
               rogue_ref_io(ROGUE_IO_FT0),
               shader->spill_staging_addr.lo32,
               rogue_ref_io(ROGUE_IO_S0),
               rogue_ref_val(spill_index))->instr;
   rogue_add_instr_comment(instr, "spill_p0");

   /* rogue_ref const4 = rogue_ref_reg(rogue_special_reg(shader, 4)); */
   rogue_ref const4 = rogue_ref_reg(rogue_const_reg(shader, 4));

   /* umadd64 r2, r3, sc4, index, base.lo32, base.hi32 */
   instr = &rogue_MADD64(b,
                shader->spill_staging_addr.lo32,
                shader->spill_staging_addr.hi32,
                const4,
                shader->spill_staging_addr.lo32,
                shader->inst_base_addr.lo32,
                shader->inst_base_addr.hi32,
                rogue_none())->instr;
   rogue_add_instr_comment(instr, "spill_p1");

   if (load) {
      rogue_backend_instr *ld = rogue_LD(b,
                                         shader->spill_staging_reg,
                                         rogue_ref_drc(0),
                                         rogue_ref_val(1),
                                         shader->spill_staging_addr.ref64);
      rogue_add_instr_commentf(&ld->instr, "spill %u ld", spill_index);
   } else {
      rogue_backend_instr *st = rogue_ST(b,
                                         shader->spill_staging_reg,
                                         rogue_ref_val(2),
                                         rogue_ref_drc(0),
                                         rogue_ref_val(1),
                                         shader->spill_staging_addr.ref64,
                                         rogue_none());
      rogue_add_instr_commentf(&st->instr, "spill %u st", spill_index);
   }
}

static bool rogue_instr_is_wdf(const rogue_instr *instr)
{
   if (instr->type != ROGUE_INSTR_TYPE_CTRL)
      return false;

   const rogue_ctrl_instr *ctrl = rogue_instr_as_ctrl(instr);
   return ctrl->op == ROGUE_CTRL_OP_WDF;
}

static void
rogue_spill_reg(rogue_shader *shader, rogue_reg *reg, unsigned spill_index)
{
   rogue_builder b;
   rogue_builder_init(&b, shader);

   /* Insert a store for the reg write. */
   assert(list_is_singular(&reg->writes));
   rogue_foreach_reg_write_safe (write, reg) {
      rogue_dst_reg_replace(write, shader->spill_staging_reg.reg);

#if 1
      rogue_instr *instr = rogue_instr_next_after_grouping(write->instr);

      /* If there's a DRC after this, insert spill code after the DRC! */
      /* TODO: get the instr drc and follow the link instead */
      if (rogue_instr_is_wdf(rogue_instr_next(instr)))
         instr = rogue_instr_next(instr);

      b.cursor = rogue_cursor_after_instr(instr);
#else
      b.cursor = rogue_cursor_after_instr_group(write->instr);
#endif
      insert_spill(&b, spill_index, false);
   }

   /* Replace uses with the loaded value. */
   rogue_foreach_reg_use_safe (use, reg) {
      b.cursor = rogue_cursor_before_instr_group(use->instr);
      insert_spill(&b, spill_index, true);

      rogue_src_reg_replace(use, shader->spill_staging_reg.reg);
   }

   /* Remove the spilled register. */
   rogue_reg_delete(reg);
}

static inline bool rogue_instr_refs_temps(const rogue_instr *instr)
{
   switch (instr->type) {
   case ROGUE_INSTR_TYPE_ALU: {
      const rogue_alu_instr *alu = rogue_instr_as_alu(instr);
      const rogue_alu_op_info *info = &rogue_alu_op_infos[alu->op];
      for (unsigned s = 0; s < info->num_srcs; ++s)
         if (rogue_ref_is_temp_reg(&alu->src[s].ref))
            return true;

      for (unsigned d = 0; d < info->num_dsts; ++d)
         if (rogue_ref_is_temp_reg(&alu->dst[d].ref))
            return true;

      break;
   }

   case ROGUE_INSTR_TYPE_BACKEND: {
      const rogue_backend_instr *backend = rogue_instr_as_backend(instr);
      const rogue_backend_op_info *info = &rogue_backend_op_infos[backend->op];
      for (unsigned s = 0; s < info->num_srcs; ++s)
         if (rogue_ref_is_temp_reg(&backend->src[s].ref))
            return true;

      for (unsigned d = 0; d < info->num_dsts; ++d)
         if (rogue_ref_is_temp_reg(&backend->dst[d].ref))
            return true;

      break;
   }

   case ROGUE_INSTR_TYPE_CTRL: {
      const rogue_ctrl_instr *ctrl = rogue_instr_as_ctrl(instr);
      const rogue_ctrl_op_info *info = &rogue_ctrl_op_infos[ctrl->op];
      for (unsigned s = 0; s < info->num_srcs; ++s)
         if (rogue_ref_is_temp_reg(&ctrl->src[s].ref))
            return true;

      for (unsigned d = 0; d < info->num_dsts; ++d)
         if (rogue_ref_is_temp_reg(&ctrl->dst[d].ref))
            return true;

      break;
   }

   case ROGUE_INSTR_TYPE_BITWISE: {
      const rogue_bitwise_instr *bitwise = rogue_instr_as_bitwise(instr);
      const rogue_bitwise_op_info *info = &rogue_bitwise_op_infos[bitwise->op];
      for (unsigned s = 0; s < info->num_srcs; ++s)
         if (rogue_ref_is_temp_reg(&bitwise->src[s].ref))
            return true;

      for (unsigned d = 0; d < info->num_dsts; ++d)
         if (rogue_ref_is_temp_reg(&bitwise->dst[d].ref))
            return true;

      break;
   }

   default:
      unreachable("Unsupported instruction type.");
      break;
   }

   return false;
}

static bool rogue_reg_can_spill(rogue_reg *reg)
{
   /* Don't spill regarrays. */
   if (reg->regarray)
      return false;

   /* Don't spill ops that already use temps.  */
   rogue_foreach_reg_use (use, reg) {
      if (rogue_instr_refs_temps(use->instr))
         return false;
   }

   rogue_foreach_reg_write (write, reg) {
      if (rogue_instr_refs_temps(write->instr))
         return false;
   }

   return true;
}

static bool rogue_regalloc_try(rogue_shader *shader)
{
   assert(!shader->is_grouped);

   unsigned num_ssa_regs = rogue_count_used_regs(shader, ROGUE_REG_CLASS_SSA);
   if (!num_ssa_regs)
      return true;

   unsigned num_temps_prealloced =
      rogue_count_used_regs(shader, ROGUE_REG_CLASS_TEMP);

   /* If we're doing control flow, allocate extra temp(s) for the execution mask
    * counter(s).
    */
   unsigned emc_reg = ROGUE_REG_UNUSED;
   unsigned num_emc_regs = rogue_count_used_regs(shader, ROGUE_REG_CLASS_EMC);
   assert(num_emc_regs <= 1); /* Should only be one for now. */
   if (num_emc_regs)
      emc_reg = num_temps_prealloced;
   num_temps_prealloced += num_emc_regs;

   /* TODO: assert that this is not negative! */
   unsigned num_hw_temps = rogue_reg_class_infos[ROGUE_REG_CLASS_TEMP].num -
                           num_temps_prealloced;

#if 1
   const char *env_temps = getenv("TEMPS");
   if (env_temps)
      num_hw_temps = atoi(env_temps) - num_temps_prealloced;
#endif

   struct ra_regs *ra_regs = ra_alloc_reg_set(shader, num_hw_temps, true);

   /* TODO: Consider tracking this in the shader itself, i.e. one list for child
    * regarrays, one for parents. Or, since children are already in a list in
    * the parent, only have parent regarrays in the shader.
    */

   /* Count the parent regarrays. */
   unsigned num_parent_regarrays = 0;
   rogue_foreach_regarray (regarray, shader) {
      if (regarray->parent || regarray->regs[0]->class != ROGUE_REG_CLASS_SSA)
         continue;

      ++num_parent_regarrays;
   }

   /* Construct list of sorted parent regarrays. */
   rogue_regarray **parent_regarrays =
      rzalloc_array_size(ra_regs,
                         sizeof(*parent_regarrays),
                         num_parent_regarrays);

   unsigned ra = 0;
   rogue_foreach_regarray (regarray, shader) {
      if (regarray->parent || regarray->regs[0]->class != ROGUE_REG_CLASS_SSA)
         continue;

      parent_regarrays[ra++] = regarray;
   }

   util_qsort_r(parent_regarrays,
                num_parent_regarrays,
                sizeof(*parent_regarrays),
                regarray_cmp,
                NULL);

   /* Setup regset and contiguous register classes. */
   struct util_sparse_array ra_class_info;
   util_sparse_array_init(&ra_class_info,
                          sizeof(rogue_ra_class_info),
                          ROGUE_RA_CLASS_INFO_NODE_SIZE);

   bool single_regs = false;
   for (unsigned u = 0; u < num_parent_regarrays; ++u) {
      /* List sorted by size! So easily skip sizes we've already handled. */
      if (u > 0 && (parent_regarrays[u]->size == parent_regarrays[u - 1]->size))
         continue;

      unsigned stride = parent_regarrays[u]->size;
      rogue_ra_setup_class(ra_regs,
                           &ra_class_info,
                           stride,
                           ROGUE_REG_CLASS_TEMP,
                           num_temps_prealloced,
                           num_hw_temps);
      if (stride == 1)
         single_regs = true;
   }

   /* Ensure we have support for single/standalone registers too. */
   if (!single_regs) {
      rogue_ra_setup_class(ra_regs,
                           &ra_class_info,
                           1,
                           ROGUE_REG_CLASS_TEMP,
                           num_temps_prealloced,
                           num_hw_temps);
   }

   rogue_ra_class_info *single_class_info =
      util_sparse_array_get(&ra_class_info, 1);
   assert(single_class_info->stride == 1);

   ra_set_finalize(ra_regs, NULL);

   /* Prepare live ranges. */
   rogue_live_range *ssa_live_range =
      rzalloc_array_size(ra_regs, sizeof(*ssa_live_range), num_ssa_regs);
   for (unsigned u = 0; u < num_ssa_regs; ++u)
      ssa_live_range[u].start = ~0U;

   /* Populate live ranges for register arrays. */
   for (unsigned u = 0; u < num_parent_regarrays; ++u) {
      rogue_regarray *regarray = parent_regarrays[u];
      unsigned base_index = regarray->regs[0]->index;
      rogue_live_range *live_range = &ssa_live_range[base_index];

      rogue_regarray_liveness(regarray, live_range);

      rogue_foreach_subarray (subarray, regarray) {
         rogue_regarray_liveness(subarray, live_range);
      }
   }

   /* Populate live ranges for registers. */
   rogue_foreach_reg (reg, shader, ROGUE_REG_CLASS_SSA) {
      if (reg->regarray)
         continue;

      rogue_live_range *live_range = &ssa_live_range[reg->index];
      rogue_reg_liveness(reg, live_range);
   }

   /* Extended lifetimes of SSA regs in loops. */
   rogue_extend_loop_reg_lifetimes(shader, ssa_live_range, num_ssa_regs);

   struct ra_graph *ra_graph =
      ra_alloc_interference_graph(ra_regs, num_ssa_regs);
   ralloc_steal(ra_regs, ra_graph);

   /* Set register class for regarrays/vectors. */
   for (unsigned u = 0; u < num_parent_regarrays; ++u) {
      rogue_regarray *regarray = parent_regarrays[u];
      unsigned base_index = regarray->regs[0]->index;
      unsigned stride = regarray->size;

      rogue_ra_class_info *class_info =
         util_sparse_array_get(&ra_class_info, stride);
      assert(class_info->stride == stride);

      ra_set_node_class(ra_graph,
                        base_index,
                        ra_get_class_from_index(ra_regs,
                                                class_info->class_index));
   }

   /* Set register class for "standalone" registers. */
   rogue_foreach_reg (reg, shader, ROGUE_REG_CLASS_SSA) {
      if (reg->regarray)
         continue;

      ra_set_node_class(
         ra_graph,
         reg->index,
         ra_get_class_from_index(ra_regs, single_class_info->class_index));
   }

   /* Build interference graph from overlapping live ranges. */
   for (unsigned index0 = 0; index0 < num_ssa_regs; ++index0) {
      rogue_live_range *live_range0 = &ssa_live_range[index0];

      for (unsigned index1 = 0; index1 < num_ssa_regs; ++index1) {
         if (index0 == index1)
            continue;

         rogue_live_range *live_range1 = &ssa_live_range[index1];

         /* If the live ranges overlap, those register nodes interfere. */
         if (!(live_range0->start >= live_range1->end ||
               live_range1->start >= live_range0->end))
            ra_add_node_interference(ra_graph, index0, index1);
      }
   }

   bool is_internal = shader->ctx->nir[shader->stage]->info.internal;
   if (!ra_allocate(ra_graph)) {
      /* abort(); */
      /* Internal shaders can't spill. */
      assert(!is_internal);

      float *spill_cost =
         ralloc_array_size(ra_regs, sizeof(float), num_ssa_regs);

      /* TODO: Move hard-coded init stuff to here, i.e. don't insert the
       * spilling setup code unless we actually need to spill.
       */

      /* Calculate spill costs. */
#if 1
      for (unsigned u = 0; u < num_ssa_regs; ++u) {
         spill_cost[u] = INFINITY;
      }
#endif

      rogue_foreach_reg (reg, shader, ROGUE_REG_CLASS_SSA) {
         unsigned i = reg->index;

         if (!rogue_reg_can_spill(reg)) {
            spill_cost[i] = INFINITY;
            continue;
         }

         unsigned uses = list_length(&reg->uses);
         /* TODO: more info to help choose which one to spill. */
         spill_cost[i] = (float)uses * 10.0f;
      }

      /* Set spill costs. */
      for (unsigned u = 0; u < num_ssa_regs; ++u)
         ra_set_node_spill_cost(ra_graph, u, spill_cost[u]);

      /* Get best spill node and spill. */
      unsigned spill_reg_idx = ra_get_best_spill_node(ra_graph);
      assert(spill_reg_idx != ~0 && "Failed to get best spill node.");
      rogue_reg *reg = rogue_ssa_reg(shader, spill_reg_idx);

      unsigned *spill_regs = &shader->ctx->common_data[shader->stage].spill_regs;
      unsigned spill_index = (*spill_regs)++;
      rogue_spill_reg(shader, reg, spill_index);

      util_sparse_array_finish(&ra_class_info);
      ralloc_free(ra_regs);

      /* Re-index instructions and regs. */
      rogue_trim(shader);

      if (ROGUE_DEBUG(REGALLOC))
         printf("Spilling attempt %u\n", spill_index);
      return false;
   }

   /* Print allocations. */
   if (ROGUE_DEBUG(REGALLOC)) {
      FILE *fp = stdout;

      /* Regarray allocations. */
      fputs("Register allocations\n", fp);
      for (unsigned u = 0; u < num_parent_regarrays; ++u) {
         rogue_regarray *regarray = parent_regarrays[u];

         unsigned size = regarray->size;
         unsigned base_index = regarray->regs[0]->index;
         unsigned hw_base_index = ra_get_node_reg(ra_graph, base_index);

         rogue_ra_class_info *class_info =
            util_sparse_array_get(&ra_class_info, size);
         assert(class_info->stride == size);
         enum rogue_reg_class new_class = class_info->hw_class;

         rogue_print_regarray(fp, regarray);
         fputs(" -> ", fp);
         rogue_print_regarray_raw(fp, new_class, hw_base_index, size);
         fputs("\n", fp);
      }

      /* Standalone register allocations. */
      rogue_foreach_reg_safe (reg, shader, ROGUE_REG_CLASS_SSA) {
         if (reg->regarray)
            continue;

         unsigned hw_index = ra_get_node_reg(ra_graph, reg->index);
         enum rogue_reg_class new_class = single_class_info->hw_class;

         rogue_print_reg(fp, reg, ROGUE_IDX_NONE);
         fputs(" -> ", fp);
         rogue_print_reg_raw(fp, new_class, hw_index);
         fputs("\n", fp);
      }

      fputs("\n", fp);
   }

   /* Replace SSA regarray registers with allocated physical registers. */
   for (unsigned u = 0; u < num_parent_regarrays; ++u) {
      rogue_regarray *regarray = parent_regarrays[u];

      unsigned base_index = regarray->regs[0]->index;
      unsigned hw_base_index = ra_get_node_reg(ra_graph, base_index);

      unsigned stride = regarray->size;
      rogue_ra_class_info *class_info =
         util_sparse_array_get(&ra_class_info, stride);
      assert(class_info->stride == stride);
      enum rogue_reg_class new_class = class_info->hw_class;

      bool used = false;
      for (unsigned r = 0; r < stride; ++r) {
         if (rogue_reg_is_used(shader, new_class, hw_base_index + r)) {
            used = true;
            break;
         }
      }

      /* First time using new regarray, modify in place. */
      if (!used) {
         rogue_regarray_rewrite(shader, regarray, new_class, hw_base_index);
      } else {
         /* Regarray has already been used, replace references and delete. */

         /* Replace parent regarray first. */
         rogue_regarray *new_regarray = rogue_regarray_cached(shader,
                                                              stride,
                                                              new_class,
                                                              hw_base_index,
                                                              true);
         rogue_regarray_replace(shader, regarray, new_regarray, true);
      }
   }

   /* Replace remaining standalone SSA registers with allocated physical
    * registers. */
   rogue_foreach_reg_safe (reg, shader, ROGUE_REG_CLASS_SSA) {
      assert(!reg->regarray);
      unsigned hw_index = ra_get_node_reg(ra_graph, reg->index);
      enum rogue_reg_class new_class = single_class_info->hw_class;

      /* First time using new register, modify in place. */
      if (!rogue_reg_is_used(shader, new_class, hw_index)) {
         rogue_reg_rewrite(shader, reg, new_class, hw_index);
      } else {
         /* Register has already been used, replace references and delete. */
         assert(list_is_singular(&reg->writes)); /* SSA reg. */
         rogue_reg *new_reg = rogue_temp_reg(shader, hw_index);
         rogue_reg_replace(reg, new_reg);
      }
   }

   rogue_foreach_reg_safe (reg, shader, ROGUE_REG_CLASS_EMC) {
      assert(!reg->regarray);
      assert(emc_reg != ROGUE_REG_UNUSED);
      unsigned hw_index = emc_reg;
      enum rogue_reg_class new_class = single_class_info->hw_class;

      /* First time using new register, modify in place. */
      if (!rogue_reg_is_used(shader, new_class, hw_index)) {
         rogue_reg_rewrite(shader, reg, new_class, hw_index);
      } else {
         /* Register has already been used, replace references and delete. */
         assert(list_is_singular(&reg->writes)); /* SSA reg. */
         rogue_reg *new_reg = rogue_temp_reg(shader, hw_index);
         rogue_reg_replace(reg, new_reg);
      }
   }

   util_sparse_array_finish(&ra_class_info);
   ralloc_free(ra_regs);

   /* In debug builds this will check the temp regs are contiguous from zero. */
   UNUSED unsigned num_temp_regs =
      rogue_count_used_regs(shader, ROGUE_REG_CLASS_TEMP);

   if (ROGUE_DEBUG(REGALLOC))
      printf("temps: %u\n", num_temp_regs);

   return true;
}

PUBLIC
bool rogue_regalloc(rogue_shader *shader)
{
   while (!rogue_regalloc_try(shader))
      ;

   return true;
}
