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
#include "nir/nir_lower_blend.h"
#include "nir/nir_search_helpers.h"
#include "rogue.h"
#include "util/macros.h"
#include "vk_graphics_state.h"
#include "vk_blend.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * \file rogue_nir_lower_vk.c
 *
 * \brief Contains Vulkan-specific NIR passes.
 */

static inline bool descriptor_is_dynamic(VkDescriptorType type)
{
   return (type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC ||
           type == VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC);
}

/* {base_addr_lo, base_addr_hi, size, offset} */
static nir_def *lower_vk_io(nir_builder *b, nir_instr *instr, void *cb_data)
{
   rogue_build_ctx *ctx = cb_data;
   const struct pvr_pipeline_layout *pipeline_layout = ctx->pipeline_layout;

   unsigned load_bits =
      nir_address_format_bit_size(nir_address_format_64bit_global);
   unsigned load_align = load_bits / 8;
   enum pvr_stage_allocation pvr_stage =
      mesa_stage_to_pvr(b->shader->info.stage);

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   assert(intr->intrinsic == nir_intrinsic_load_vulkan_descriptor);

   nir_intrinsic_instr *vk_res_idx = nir_src_as_intrinsic(intr->src[0]);
   assert(vk_res_idx->intrinsic == nir_intrinsic_vulkan_resource_index);

   /* Fetch the desc_set, binding, desc_type. */
   unsigned desc_set = nir_intrinsic_desc_set(vk_res_idx);
   unsigned binding = nir_intrinsic_binding(vk_res_idx);
   /* VkDescriptorType desc_type = nir_intrinsic_desc_type(vk_res_idx); */

   /* TODO: Skip VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER and other types
    * that will be put into shareds. Rather than hardcoding types,
    * look at info passed by the driver.
    * For types that live in memory, we can probably skip the custom intrinsics
    * -> lowering to load_global and instead just load_global directly.
    */

   b->cursor = nir_before_instr(&intr->instr);

   /* This will be handled in rogue_nir_to_rogue, load the base address from
    * shareds. */
   nir_def *tbl_base_addr = nir_load_vulkan_desc_set_table_base_addr_img(b);
   assert(desc_set < pipeline_layout->set_count);

   /* Calculate offset for the descriptor set. */
   unsigned desc_set_offset = desc_set * sizeof(pvr_dev_addr_t);

   /* Load the descriptor set table address for this set from memory. */
   nir_def *tbl_addr =
      nir_load_global_constant(b,
                               nir_iadd_imm(b, tbl_base_addr, desc_set_offset),
                               load_align,
                               1,
                               load_bits);

   const struct pvr_descriptor_set_layout *set_layout =
      pipeline_layout->set_layout[desc_set];
   const struct pvr_descriptor_set_layout_mem_layout *mem_layout =
      &set_layout->memory_layout_in_dwords_per_stage[pvr_stage];

   /* Calculate offset for the descriptor/binding in this set. */
   const struct pvr_descriptor_set_layout_binding *binding_layout =
      pvr_get_descriptor_binding(set_layout, binding);

   assert(binding_layout);

   /* TODO: Handle secondaries. */

   unsigned desc_offset = descriptor_is_dynamic(binding_layout->type)
                             ? set_layout->total_size_in_dwords
                             : mem_layout->primary_offset;

   desc_offset += binding_layout->per_stage_offset_in_dwords[pvr_stage].primary;

   /* Add the offset of the descriptor within the binding. */
   struct pvr_descriptor_size_info desc_size_info;
   pvr_descriptor_size_info_init(ctx->compiler->dev_info,
                                 pipeline_layout->robust_buffer_access,
                                 binding_layout->type,
                                 &desc_size_info);

   assert(load_bits / 32 == desc_size_info.primary);

   unsigned desc_elem = nir_src_as_uint(vk_res_idx->src[0]);
   desc_offset += desc_elem * desc_size_info.primary;

   desc_offset = PVR_DW_TO_BYTES(desc_offset);

   unsigned comps = intr->def.num_components;
   unsigned bits = intr->def.bit_size;
   unsigned align = bits / 8;

   assert((bits == 64 && comps == 1) || (bits == 32 && comps == 4));
   bool is_ubo = (bits == 64);

   /* Load the descriptor set table address for this set from memory. */
   nir_def *desc_addr;
   if (is_ubo) {
      desc_addr =
         nir_load_global_constant(b,
                                  nir_iadd_imm(b, tbl_addr, desc_offset),
                                  align,
                                  comps,
                                  bits);
   } else {
      assert(bits / 32 == desc_size_info.secondary);

      unsigned desc_size_offset = descriptor_is_dynamic(binding_layout->type)
                                     ? (set_layout->total_size_in_dwords +
                                        mem_layout->primary_dynamic_size)
                                     : mem_layout->secondary_offset;
      desc_size_offset +=
         binding_layout->per_stage_offset_in_dwords[pvr_stage].secondary;
      desc_size_offset += desc_elem * desc_size_info.secondary;
      desc_size_offset = PVR_DW_TO_BYTES(desc_size_offset);

      desc_addr =
         nir_load_global_constant(b,
                                  nir_iadd_imm(b, tbl_addr, desc_offset),
                                  align,
                                  2,
                                  bits);

      nir_def *desc_size =
         nir_load_global_constant(b,
                                  nir_iadd_imm(b, tbl_addr, desc_size_offset),
                                  align,
                                  1,
                                  bits);

      /* TODO: When re-doing descriptor (sets), we can store the buffer base
       * address, offset, and size, and load all three. */
      unsigned offset = 0;
      nir_def *desc_offset = nir_imm_int(b, offset);

      nir_def *components[4] = {
         nir_channel(b, desc_addr, 0),
         nir_channel(b, desc_addr, 1),
         desc_size,
         desc_offset,
      };
      desc_addr = nir_vec(b, components, ARRAY_SIZE(components));
   }

   return desc_addr;
}

static bool is_vk_io(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   return intr->intrinsic == nir_intrinsic_load_vulkan_descriptor;
}

PUBLIC
bool rogue_nir_lower_vk_io(nir_shader *shader, rogue_build_ctx *ctx)
{
   return nir_shader_lower_instructions(shader, is_vk_io, lower_vk_io, ctx);
}

PUBLIC
void rogue_nir_lower_blend(nir_shader *shader, rogue_build_ctx *ctx)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   struct rogue_fs_build_data *fs_data = &ctx->stage_data.fs;
   const struct vk_color_blend_state *cb_state = fs_data->cb_state;

   /* No blending info given, skip it */
   if (!cb_state)
      return;

   nir_lower_blend_options opts = {
      .scalar_blend_const = false, /* TODO!!! */
      .logicop_enable = cb_state->logic_op_enable,
      .logicop_func = vk_logic_op_to_pipe(cb_state->logic_op),
      .skip_blend_factor_snorm_clamp = true,
   };

   memset(opts.format, 0, sizeof(*opts.format));

   unsigned count = MIN2(fs_data->num_outputs, cb_state->attachment_count);
   for (unsigned u = 0; u < count; ++u) {
      const struct vk_color_blend_attachment_state *rt =
         &cb_state->attachments[u];
      opts.format[u] = fs_data->outputs[u].format;

      if (cb_state->logic_op_enable) {
         /* No blending, but we get the colour mask below */
      } else if (!rt->blend_enable) {
         static const nir_lower_blend_channel replace = {
            .func = PIPE_BLEND_ADD,
            .src_factor = PIPE_BLENDFACTOR_ONE,
            .dst_factor = PIPE_BLENDFACTOR_ZERO,
         };

         opts.rt[u].rgb = replace;
         opts.rt[u].alpha = replace;
      } else {
         opts.rt[u].rgb.func = vk_blend_op_to_pipe(rt->color_blend_op);
         opts.rt[u].rgb.src_factor =
            vk_blend_factor_to_pipe(rt->src_color_blend_factor);
         opts.rt[u].rgb.dst_factor =
            vk_blend_factor_to_pipe(rt->dst_color_blend_factor);

         opts.rt[u].alpha.func = vk_blend_op_to_pipe(rt->alpha_blend_op);
         opts.rt[u].alpha.src_factor =
            vk_blend_factor_to_pipe(rt->src_alpha_blend_factor);
         opts.rt[u].alpha.dst_factor =
            vk_blend_factor_to_pipe(rt->dst_alpha_blend_factor);
      }

      opts.rt[u].colormask =
         (cb_state->color_write_enables & BITFIELD_BIT(u)) ? rt->write_mask : 0;
   }

   return nir_lower_blend(shader, &opts);
}

static bool is_blend_const(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   return intr->intrinsic == nir_intrinsic_load_blend_const_color_rgba;
}

static nir_def *lower_static_blend_const(nir_builder *b,
                                         const rogue_build_ctx *ctx)
{
   const struct vk_color_blend_state *cb_state = ctx->stage_data.fs.cb_state;
   nir_const_value blend_consts[4];
   const unsigned bit_size = 32;

   for (unsigned c = 0; c < ARRAY_SIZE(blend_consts); ++c)
      blend_consts[c] =
         nir_const_value_for_float(cb_state->blend_constants[c], bit_size);

   return nir_build_imm(b, ARRAY_SIZE(blend_consts), bit_size, blend_consts);
}

static nir_def *lower_dynamic_blend_const(nir_builder *b)
{
   nir_def *consts_base = nir_load_blend_consts_base_addr_img(b);
   const unsigned bit_size = 32;
   const unsigned align = bit_size / 8;

   nir_def *blend_consts[4];
   for (unsigned c = 0; c < ARRAY_SIZE(blend_consts); ++c) {
      blend_consts[c] =
         nir_load_global_constant(b,
                                  nir_iadd_imm(b, consts_base, align * c),
                                  align,
                                  1,
                                  bit_size);
   }

   return nir_vec(b, blend_consts, ARRAY_SIZE(blend_consts));
}

static nir_def *
lower_blend_const(nir_builder *b, nir_instr *instr, void *cb_data)
{
   const rogue_build_ctx *ctx = cb_data;

   const struct pvr_pipeline_layout *pipeline_layout = ctx->pipeline_layout;
   const struct pvr_sh_reg_layout *sh_reg_layout =
      &pipeline_layout->sh_reg_layout_per_stage[PVR_STAGE_ALLOCATION_FRAGMENT];

   b->cursor = nir_before_instr(instr);

   if (!sh_reg_layout->blend_consts.present)
      return lower_static_blend_const(b, ctx);

   return lower_dynamic_blend_const(b);
}

PUBLIC
bool rogue_nir_lower_blend_consts(nir_shader *shader, rogue_build_ctx *ctx)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   return nir_shader_lower_instructions(shader,
                                        is_blend_const,
                                        lower_blend_const,
                                        ctx);
}

// TODO: Move into the right file!
static nir_def *lower_scratch(nir_builder *b, nir_instr *instr, UNUSED void *cb_data)
{
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   bool is_load = intr->intrinsic == nir_intrinsic_load_scratch;

   unsigned num_components = is_load ? intr->def.num_components : nir_src_num_components(intr->src[0]);
   /* assert(num_components == 1); */

   unsigned bit_size = is_load ? intr->def.bit_size : nir_src_bit_size(intr->src[0]);
   assert(bit_size == 32);

   unsigned align = bit_size / 8;

   nir_def *offset = intr->src[is_load ? 0 : 1].ssa;
   nir_def *scratch_base = nir_load_scratch_base_ptr(b, 1, 64, .base = 1);
   nir_def *addr = nir_iadd(b, scratch_base, nir_u2u64(b, offset));

   if (is_load)
      return nir_load_global(b, addr, align, num_components, bit_size);

   nir_def *value = intr->src[0].ssa;
   nir_store_global(b, addr, align, value, BITFIELD_MASK(num_components));

   return NIR_LOWER_INSTR_PROGRESS_REPLACE;
}

static bool is_scratch(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   return intr->intrinsic == nir_intrinsic_load_scratch || intr->intrinsic == nir_intrinsic_store_scratch;
}

PUBLIC
bool rogue_nir_lower_scratch(nir_shader *shader)
{
   return nir_shader_lower_instructions(shader, is_scratch, lower_scratch, NULL);
}
