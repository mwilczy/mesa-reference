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
 * \file rogue_nir_lower_vk_io.c
 *
 * \brief Contains the rogue_nir_lower_vk_io pass.
 */

static inline bool descriptor_is_dynamic(VkDescriptorType type)
{
   return (type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC ||
           type == VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC);
}

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
   assert(binding < set_layout->binding_count);

   /* Calculate offset for the descriptor/binding in this set. */
   const struct pvr_descriptor_set_layout_binding *binding_layout =
      pvr_get_descriptor_binding(set_layout, binding);

   /* TODO: Handle secondaries. */

   unsigned desc_offset = descriptor_is_dynamic(binding_layout->type)
                             ? set_layout->total_size_in_dwords
                             : mem_layout->primary_offset;

   desc_offset += binding_layout->per_stage_offset_in_dwords[pvr_stage].primary;

   /* Add the offset of the descriptor within the binding. */
   if (binding_layout->descriptor_count > 1) {
      unsigned desc_elem = nir_src_as_uint(vk_res_idx->src[0]);

      struct pvr_descriptor_size_info desc_size_info;
      pvr_descriptor_size_info_init(ctx->compiler->dev_info,
                                    pipeline_layout->robust_buffer_access,
                                    binding_layout->type,
                                    &desc_size_info);

      desc_offset += desc_elem * desc_size_info.primary;
   }

   desc_offset = PVR_DW_TO_BYTES(desc_offset);

   /* Load the descriptor set table address for this set from memory. */
   nir_def *desc_addr =
      nir_load_global_constant(b,
                               nir_iadd_imm(b, tbl_addr, desc_offset),
                               load_align,
                               intr->def.num_components,
                               intr->def.bit_size);

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
