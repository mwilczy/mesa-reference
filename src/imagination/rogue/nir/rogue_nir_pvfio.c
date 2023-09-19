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
 * \file rogue_nir_pvfio.c
 *
 * \brief Contains per-vertex/fragment input/output passes.
 */

/* Vertex-specific. */

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

   /* const struct util_format_description *fmt_desc = &inputs->format_descs[l];
    */
   const struct util_format_description *fmt_desc =
      util_format_description(inputs->format[l]);

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
   unsigned chan_vtxin_reg = base_vtxin_reg + vtxin_offset;

   /* Make sure the channel doesn't span multiple registers. */
   assert(vtxin_offset ==
          ((chan_desc->shift + chan_desc->size - 1) / ROGUE_REG_SIZE_BITS));

   /* TODO: Ensure the number of var_shader_ins match the attribs
    * (mats get split).
    */
   /* TODO: Update the var_shader_in types? */

   /* Load vertex input register. */
   nir_def *raw_load = nir_load_input(b,
                                      load_components,
                                      ROGUE_REG_SIZE_BITS,
                                      nir_imm_int(b, 0),
                                      .base = chan_vtxin_reg,
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
#if 1
   chan %= DIV_ROUND_UP(fmt_chans, num_vtxin_regs);
#else
   assert(!(fmt_chans % num_vtxin_regs));
   chan %= (fmt_chans / num_vtxin_regs);
#endif

   nir_def *result = fmt_unpack_scalar(b, raw_load, bit_size, chan, chan_desc);
   result = fmt_colorspace_transform_scalar(b,
                                            result,
                                            bit_size,
                                            chan,
                                            fmt_desc,
                                            false);
   return result;
}

static bool is_vert_in(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_load_input)
      return false;

   nir_io_semantics io_sem = nir_intrinsic_io_semantics(intr);
   assert(io_sem.location >= VERT_ATTRIB_GENERIC0 &&
          io_sem.location <= VERT_ATTRIB_GENERIC15);

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

/* Fragment-specific. */

static nir_def *build_tiled_address(nir_builder *b,
                                    const struct usc_mrt_resource *mrt_resource,
                                    unsigned fragout_offset,
                                    nir_def **coverage_mask)
{
   assert(mrt_resource->type == USC_MRT_RESOURCE_TYPE_MEMORY);

   /* Only stores have coverage masks. */
   bool is_store = (coverage_mask != NULL);
   unsigned channel = mrt_resource->mem.offset_dw + fragout_offset;

   nir_def *base_addr =
      nir_load_tile_buffer_base_addr_img(b,
                                         .base = mrt_resource->mem.tile_buffer);
   nir_def *offset =
      nir_load_tile_buffer_offset_img(b,
                                      .base = channel,
                                      .tile_buffer_store_img = is_store);
   nir_def *address = nir_iadd(b, base_addr, nir_u2u64(b, offset));

   /* TODO: MSAA mask will need to be handled differently. */
   if (is_store)
      *coverage_mask = nir_ishl(b, nir_imm_int(b, 1), nir_load_sample_id(b));

   return address;
}

static bool is_frag_out(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_store_output &&
       intr->intrinsic != nir_intrinsic_load_output)
      return false;

   nir_io_semantics io_sem = nir_intrinsic_io_semantics(intr);
   assert(io_sem.dual_source_blend_index == 0 && "Should have been lowered");

   /* TODO: Depth, sample mask, etc. support. */
   if (io_sem.location < FRAG_RESULT_DATA0 ||
       io_sem.location > FRAG_RESULT_DATA7)
      return false;

   /* Shouldn't have any stores in control flow */
   assert(instr->block->cf_node.parent->type == nir_cf_node_function);

   return true;
}

struct pfo_state {
   struct rogue_fs_build_data *fs_data;

   /* [FRAG_RESULT_DATA0..7][dword]
    * (max block size is 256 bits = 32 bytes = 8 dwords)
    * TODO: find existing defines to use */

   /* Track the last stores/pack to a register so we can merge them as we go.
    * This ensures that by the end there is only a single write to each pixel
    * output register/buffer DWORD.
    */
   struct {
      struct {
         /* The last store operation (last pack is in src[0]). */
         nir_intrinsic_instr *intr;
      } reg[8];
   } last_store[MAX_DRAW_BUFFERS];

   /* The first store at the end of the shader. */
   nir_instr *first_store;

   /* Condition accumulator for multiple discards. */
   nir_def *discard_cond_accum;

   /* Src for depth feedback (undef if unused). */
   nir_def *depth_feedback_src;
};

static struct util_format_description *
pbe_fmt_desc(nir_builder *b, enum pipe_format format, nir_def **value)
{
   bool is_store = value != NULL;

   struct util_format_description *fmt_desc = util_format_description(format);

   enum util_format_type type = UTIL_FORMAT_TYPE_VOID;
   unsigned bits = 0;
   unsigned comps = fmt_desc->nr_channels;
   bool is_norm = fmt_desc->is_unorm || fmt_desc->is_snorm;
   bool is_pure_int = false; /* TODO */
   bool is_alt = false;
   bool needs_sat = false;

   switch (fmt_desc->colorspace) {
   case UTIL_FORMAT_COLORSPACE_SRGB:
      is_alt = true;
      type = UTIL_FORMAT_TYPE_FLOAT;
      bits = 16;
      /* TODO NEXT: just for SRGB or for all F16? */
      needs_sat = is_store && is_norm;
      break;

   case UTIL_FORMAT_COLORSPACE_RGB:
      switch (fmt_desc->format) {
      case PIPE_FORMAT_B5G6R5_UNORM:
      case PIPE_FORMAT_R5G6B5_UNORM:
      case PIPE_FORMAT_B8G8R8A8_UNORM:
      /* case PIPE_FORMAT_A4B4G4R4_UNORM: */
      case PIPE_FORMAT_A4R4G4B4_UNORM:
      case PIPE_FORMAT_B5G5R5A1_UNORM:
         is_alt = true;
         type = UTIL_FORMAT_TYPE_UNSIGNED;
         bits = 8;
         break;

      case PIPE_FORMAT_R10G10B10A2_UNORM:
      case PIPE_FORMAT_R11G11B10_FLOAT:
         is_alt = true;
         type = UTIL_FORMAT_TYPE_FLOAT;
         bits = 16;
         /* needs_sat = is_store && is_norm; */
         break;

      case PIPE_FORMAT_R9G9B9E5_FLOAT:
         abort();
         break;

      default:
         break;
      }

   default:
      break;
   }

   /* No change required. */
   if (!is_alt)
      return fmt_desc;

   if (needs_sat) {
      *value = fmt_desc->is_unorm ? nir_fsat(b, *value)
                                  : nir_fsat_signed(b, *value);
   }

   enum pipe_format pbe_fmt =
      util_format_get_array(type, bits, comps, is_norm, is_pure_int);

   fmt_desc = util_format_description(pbe_fmt);

   return fmt_desc;
}

#if 0
static struct util_format_description *
fmt_to_pbe(struct util_format_description *fmt_desc,
           nir_builder *b,
           nir_def **value)
{
   bool is_store = value != NULL;

   enum util_format_type type = UTIL_FORMAT_TYPE_VOID;
   unsigned bits = 0;
   unsigned comps = fmt_desc->nr_channels;
   bool is_norm = fmt_desc->is_unorm || fmt_desc->is_snorm;
   bool is_pure_int = false; /* TODO */
   bool is_alt = false;
   bool needs_sat = false;

   switch (fmt_desc->colorspace) {
   case UTIL_FORMAT_COLORSPACE_SRGB:
      is_alt = true;
      type = UTIL_FORMAT_TYPE_FLOAT;
      bits = 16;
      /* TODO NEXT: just for SRGB or for all F16? */
      needs_sat = is_store && is_norm;
      break;

   case UTIL_FORMAT_COLORSPACE_RGB:
      switch (fmt_desc->format) {
      case PIPE_FORMAT_B5G6R5_UNORM:
      case PIPE_FORMAT_R5G6B5_UNORM:
      case PIPE_FORMAT_B8G8R8A8_UNORM:
      /* case PIPE_FORMAT_A4B4G4R4_UNORM: */
      case PIPE_FORMAT_A4R4G4B4_UNORM:
      case PIPE_FORMAT_B5G5R5A1_UNORM:
         is_alt = true;
         type = UTIL_FORMAT_TYPE_UNSIGNED;
         bits = 8;
         break;

      case PIPE_FORMAT_R10G10B10A2_UNORM:
      case PIPE_FORMAT_R11G11B10_FLOAT:
         is_alt = true;
         type = UTIL_FORMAT_TYPE_FLOAT;
         bits = 16;
         /* needs_sat = is_store && is_norm; */
         break;

      case PIPE_FORMAT_R9G9B9E5_FLOAT:
         abort();
         break;

      default:
         break;
      }

   default:
      break;
   }

   /* printf("pbe orig fmt: %s. change: %c\n", fmt_desc->name, is_alt ? 'y' :
    * 'n'); */

   /* No change required. */
   if (!is_alt)
      return fmt_desc;

   if (needs_sat) {
      *value = fmt_desc->is_unorm ? nir_fsat(b, *value)
                                  : nir_fsat_signed(b, *value);
   }

   enum pipe_format pbe_fmt =
      util_format_get_array(type, bits, comps, is_norm, is_pure_int);

   const struct util_format_description *pbe_desc =
      util_format_description(pbe_fmt);

   /* printf("pbe alt fmt: %s\n", pbe_desc->name); */

   return pbe_desc;
}
#endif

static nir_def *lower_frag_store_out(nir_builder *b,
                                     nir_intrinsic_instr *intr,
                                     struct pfo_state *state)
{
   const struct rogue_fs_build_data *fs_data = state->fs_data;

   b->cursor = nir_before_instr(&intr->instr);

   nir_def *value = intr->src[0].ssa;
   assert(nir_src_as_uint(intr->src[1]) == 0); /* TODO: Probably won't be the
                                                  case! Catch this and figure
                                                  out the mappings. */

   unsigned bit_size = nir_src_bit_size(intr->src[0]);

   unsigned store_components = nir_src_num_components(intr->src[0]);
   assert(store_components == 1);

   nir_io_semantics io_sem = nir_intrinsic_io_semantics(intr);
   unsigned l = io_sem.location - FRAG_RESULT_DATA0;
   assert(l < fs_data->num_outputs);

   /* struct util_format_description *fmt_desc = &fs_data->outputs[l].fmt_desc;
    */
   /* struct util_format_description *fmt_desc =
    * util_format_description(fs_data->outputs[l].format); */
   enum pipe_format format = fs_data->outputs[l].format;
   struct util_format_description *fmt_desc = pbe_fmt_desc(b, format, &value);

   /* Number of fragment output registers that will contain the format. */
   unsigned num_fragout_regs = PVR_BITS_TO_DW(fmt_desc->block.bits);

   const struct usc_mrt_resource *mrt_resource =
      fs_data->outputs[l].mrt_resource;
   assert(mrt_resource);

   unsigned pbe_dwords = PVR_BYTES_TO_DW(mrt_resource->intermediate_size);
   /* TODO: If we always pass this, can probably  get rid of pbe_dwords. */
   assert(pbe_dwords == num_fragout_regs);

   unsigned store_component = nir_intrinsic_component(intr);
   enum pipe_swizzle chan = fmt_desc->swizzle[store_component];
   if (chan > PIPE_SWIZZLE_W)
      return NIR_LOWER_INSTR_PROGRESS_REPLACE;

   unsigned fmt_chans = fmt_desc->nr_channels;
   assert(chan < fmt_chans);

   const struct util_format_channel_description *chan_desc =
      &fmt_desc->channel[chan];

   unsigned chan_offset = chan_desc->shift;
   unsigned chan_bits = chan_desc->size;
   /* No support for 64-bit components (yet). */
   assert(chan_bits <= 32);

   /* Fragment output register offset that contains this channel. */
   unsigned fragout_offset = chan_offset / ROGUE_REG_SIZE_BITS;

   /* Make sure the channel doesn't span multiple registers. */
   assert(fragout_offset ==
          ((chan_offset + chan_bits - 1) / ROGUE_REG_SIZE_BITS));

   /* Prepare the base/offset for the store_output op. */
   unsigned base;
   nir_def *offset;
   /* On-chip register store. */
   if (mrt_resource->type == USC_MRT_RESOURCE_TYPE_OUTPUT_REG) {
      assert(mrt_resource->reg.offset == 0);

      base = mrt_resource->reg.output_reg + fragout_offset;
      offset = nir_imm_int(b, 0);
   }
   /* Off-chip tile buffer store. */
   else if (USC_MRT_RESOURCE_TYPE_MEMORY) {
      nir_def *cov_msk;
      nir_def *addr =
         build_tiled_address(b, mrt_resource, fragout_offset, &cov_msk);

      base = 0;
      offset = nir_pass_cov_mask_img(b, addr, cov_msk);
   } else {
      unreachable("Invalid MRT resource type.");
   }

   /* Adjust the channel component.
    * E.g. for [R16G16][B16A16]:
    * var.r => [0].x
    * var.g => [0].y
    * var.b => [1].x
    * var.a => [1].y
    */
   assert(fmt_chans >= num_fragout_regs);
   chan %= DIV_ROUND_UP(fmt_chans, num_fragout_regs);

   /* Prepare store arguments. */
   nir_def *pack_base = nir_imm_int(b, 0);
   unsigned write_mask = (1 << chan);
   unsigned component = chan;

   /* If there has already been a store to this component, remove it and merge
    * its packing with this one. */
   nir_intrinsic_instr **last_store =
      &state->last_store[l].reg[fragout_offset].intr;
   if (*last_store) {
      pack_base = (*last_store)->src[0].ssa;
      write_mask |= nir_intrinsic_write_mask(*last_store);
      component = MIN2(component, nir_intrinsic_component(*last_store));
      nir_instr_remove(&(*last_store)->instr);
   }

   /* Transform and pack the value to be stored. */
   value =
      fmt_colorspace_transform_scalar(b, value, bit_size, chan, fmt_desc, true);
   value = fmt_pack_scalar(b,
                           pack_base,
                           value,
                           bit_size,
                           nir_imm_int(b, chan),
                           chan_desc,
                           false);

   /* srcs:
    * - 0: value
    * - 1: offset = nir_pass_cov_mask_img(tile_addr, cov_msk), or const 0 if
    * pixel output register.
    *
    * indices:
    * - base = pixel output register or ignored if offset is non-const
    * - write_mask = which component is being written to
    * - component = also which component is being written to?
    * - src_type = invalid, can't represent exotic bit types
    */

   nir_intrinsic_instr *store =
      nir_store_output(b,
                       value,
                       offset,
                       .base = base,
                       .write_mask = 1,
                       .component = component,
                       .src_type = nir_type_invalid,
                       .io_semantics = io_sem,
                       .io_xfb = nir_intrinsic_io_xfb(intr),
                       .io_xfb2 = nir_intrinsic_io_xfb2(intr));

   *last_store = store;

   return NIR_LOWER_INSTR_PROGRESS_REPLACE;
}

static nir_def *lower_frag_load_out(nir_builder *b,
                                    nir_intrinsic_instr *intr,
                                    struct pfo_state *state)
{
   const struct rogue_fs_build_data *fs_data = state->fs_data;

   b->cursor = nir_before_instr(&intr->instr);

   assert(nir_src_as_uint(intr->src[0]) == 0); /* TODO: Probably won't be the
                                                  case! Catch this and figure
                                                  out the mappings. */

   unsigned bit_size = nir_src_bit_size(intr->src[0]);

   unsigned store_components = intr->def.num_components;
   assert(store_components == 1);

   nir_io_semantics io_sem = nir_intrinsic_io_semantics(intr);
   /* bool is_input_attachment = io_sem.fb_fetch_output; */
   unsigned l = io_sem.location - FRAG_RESULT_DATA0;
   /* assert((!is_input_attachment && l < fs_data->num_outputs) || (is_input_attachment && l < fs_data->num_inputs)); */
   assert(l < fs_data->num_outputs);

   /* struct util_format_description *fmt_desc = &fs_data->outputs[l].fmt_desc;
    */
   /* struct util_format_description *fmt_desc =
    * util_format_description(fs_data->outputs[l].format); */
   enum pipe_format format = fs_data->outputs[l].format;
   struct util_format_description *fmt_desc = pbe_fmt_desc(b, format, NULL);

   /* Number of fragment output registers that will contain the format. */
   unsigned num_fragout_regs = PVR_BITS_TO_DW(fmt_desc->block.bits);

   const struct usc_mrt_resource *mrt_resource =
      fs_data->outputs[l].mrt_resource;
   assert(mrt_resource);

   unsigned pbe_dwords = PVR_BYTES_TO_DW(mrt_resource->intermediate_size);
   /* TODO: If we always pass this, can probably  get rid of pbe_dwords. */
   assert(pbe_dwords == num_fragout_regs);

   unsigned store_component = nir_intrinsic_component(intr);
   enum pipe_swizzle chan = fmt_desc->swizzle[store_component];
   /* Unspecified components. */
   if (chan > PIPE_SWIZZLE_W)
      return get_unspec_chan(b, chan, bit_size, fmt_desc);

   unsigned fmt_chans = fmt_desc->nr_channels;
   assert(chan < fmt_chans);

   const struct util_format_channel_description *chan_desc =
      &fmt_desc->channel[chan];

   unsigned chan_offset = chan_desc->shift;
   unsigned chan_bits = chan_desc->size;
   /* No support for 64-bit components (yet). */
   assert(chan_bits <= 32);

   /* Fragment output register offset that contains this channel. */
   unsigned fragout_offset = chan_offset / ROGUE_REG_SIZE_BITS;

   /* Make sure the channel doesn't span multiple registers. */
   assert(fragout_offset ==
          ((chan_offset + chan_bits - 1) / ROGUE_REG_SIZE_BITS));

   /* Prepare the base/offset for the store_output op. */
   unsigned base;
   nir_def *offset;
   /* On-chip register store. */
   if (mrt_resource->type == USC_MRT_RESOURCE_TYPE_OUTPUT_REG) {
      assert(mrt_resource->reg.offset == 0);

      base = mrt_resource->reg.output_reg + fragout_offset;
      offset = nir_imm_int(b, 0);
   }
   /* Off-chip tile buffer store. */
   else if (USC_MRT_RESOURCE_TYPE_MEMORY) {
      nir_def *addr =
         build_tiled_address(b, mrt_resource, fragout_offset, NULL);

      base = 0;
      offset = addr;
   } else {
      unreachable("Invalid MRT resource type.");
   }

   /* Adjust the channel component.
    * E.g. for [R16G16][B16A16]:
    * var.r => [0].x
    * var.g => [0].y
    * var.b => [1].x
    * var.a => [1].y
    */
   assert(fmt_chans >= num_fragout_regs);
   chan %= DIV_ROUND_UP(fmt_chans, num_fragout_regs);

   nir_def *raw_load = nir_load_output(b,
                                       store_components,
                                       ROGUE_REG_SIZE_BITS,
                                       offset,
                                       .base = base,
                                       .component = 0,
                                       .dest_type = nir_type_invalid,
                                       .io_semantics = io_sem);

   nir_def *result = fmt_unpack_scalar(b, raw_load, bit_size, chan, chan_desc);
   result = fmt_colorspace_transform_scalar(b,
                                            result,
                                            bit_size,
                                            chan,
                                            fmt_desc,
                                            false);
   return result;
}

static nir_def *lower_frag_out(nir_builder *b, nir_instr *instr, void *cb_data)
{
   struct pfo_state *state = (struct pfo_state *)cb_data;
   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);

   switch (intr->intrinsic) {
   case nir_intrinsic_store_output:
      return lower_frag_store_out(b, intr, state);

   case nir_intrinsic_load_output:
      return lower_frag_load_out(b, intr, state);

   default:
      break;
   }

   unreachable("Unsupported frag out op.");
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

static bool sink_frag_outs(nir_shader *shader, struct pfo_state *state)
{
   bool progress = false;

   nir_instr *after_instr = nir_block_last_instr(
      nir_impl_last_block(nir_shader_get_entrypoint(shader)));

   for (unsigned l = 0; l < ARRAY_SIZE(state->last_store); ++l) {
      for (unsigned r = 0; r < ARRAY_SIZE(state->last_store->reg); ++r) {
         nir_intrinsic_instr *intr = state->last_store[l].reg[r].intr;
         if (!intr)
            continue;

         progress |= nir_instr_move(nir_after_instr(after_instr), &intr->instr);
         if (!state->first_store)
            state->first_store = &intr->instr;

         after_instr = &intr->instr;
      }
   }

   return progress;
}

static bool is_discard(const nir_instr *instr)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_discard && intr->intrinsic != nir_intrinsic_discard_if) {
      return false;
   }

   /* Instructions shouldn't be in any control flow. */
   /* assert(instr->block->cf_node.parent->type == nir_cf_node_function); */

   return true;
}

static bool
lower_discard(struct nir_builder *b, nir_instr *instr, void *data)
{
   if (!is_discard(instr))
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   struct pfo_state *state = data;

   b->cursor = nir_after_instr(instr);

   switch (intr->intrinsic) {
   case nir_intrinsic_discard:
      state->discard_cond_accum =
         nir_ior(b, state->discard_cond_accum, nir_imm_true(b));
      break;

   case nir_intrinsic_discard_if:
      state->discard_cond_accum =
         nir_ior(b, state->discard_cond_accum, intr->src[0].ssa);
      break;

   default:
      unreachable();
   }

   nir_instr_remove(instr);
   return true;
}

static bool lower_isp_fb(nir_builder *b, struct pfo_state *state)
{
   bool has_depth_feedback = state->depth_feedback_src->parent_instr->type != nir_instr_type_undef;
   /* TODO: Sample mask support. */

   bool progress = false;

   progress |= nir_shader_instructions_pass(b->shader,
                                            lower_discard,
                                            nir_metadata_block_index |
                                               nir_metadata_dominance,
                                            state);
   if (!progress && !has_depth_feedback)
      return false;

   /* Insert isp feedback instruction before the first store,
    * or if there are no stores, at the end.
    */
   if (state->first_store)
      b->cursor = nir_before_instr(state->first_store);
   else
      b->cursor = nir_after_block(
         nir_impl_last_block(nir_shader_get_entrypoint(b->shader)));

   nir_isp_feedback_img(b,
                        state->discard_cond_accum,
                        state->depth_feedback_src);

   /* TODO: Change where lower undef pass is used such that this check can happen outside of this pass. */
   if (state->depth_feedback_src->parent_instr->type != nir_instr_type_undef)
      state->fs_data->depth_feedback = true;

   /* TODO: try to fold the condition check into isp_feedback_img
    * so that ACMP can be used
    */

   return progress;
}

static bool is_depth_feedback(const nir_instr *instr)
{
   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   if (intr->intrinsic != nir_intrinsic_store_output)
      return false;

   nir_io_semantics sem = nir_intrinsic_io_semantics(intr);
   if (sem.location != FRAG_RESULT_DEPTH)
      return false;

   assert(nir_src_num_components(intr->src[0]) == 1);
   assert(nir_src_as_uint(intr->src[1]) == 0);

   /* Instructions shouldn't be in any control flow. */
   /* assert(instr->block->cf_node.parent->type == nir_cf_node_function); */

   return true;
}

static bool
_lower_depth_feedback(struct nir_builder *b, nir_instr *instr, void *data)
{
   if (!is_depth_feedback(instr))
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   struct pfo_state *state = data;

   b->cursor = nir_after_instr(instr);

   assert(state->depth_feedback_src->parent_instr->type == nir_instr_type_undef);
   state->depth_feedback_src = intr->src[0].ssa;

   nir_instr_remove(instr);
   return true;
}

static bool lower_depth_feedback(nir_builder *b, struct pfo_state *state)
{
   bool progress = false;

   progress |= nir_shader_instructions_pass(b->shader,
                                            _lower_depth_feedback,
                                            nir_metadata_block_index |
                                               nir_metadata_dominance,
                                            state);

   /* TODO NEXT: pfo currently happens before other i/o is lowered so instead of just checking shader info for whether memory is written to,
    * we have to do this whole song and dance. Move pass to later, and make sure that the smp write variant also sets the "writes to memory". */
   bool has_depth_feedback = state->depth_feedback_src->parent_instr->type != nir_instr_type_undef;
   if (state->fs_data->side_effects && !has_depth_feedback) {
      nir_variable *var_pos = nir_find_variable_with_location (b->shader, nir_var_shader_in, VARYING_SLOT_POS);

      if (!var_pos) {
         var_pos = nir_create_variable_with_location(b->shader,
               nir_var_shader_in,
               VARYING_SLOT_POS,
               glsl_vec4_type());
         var_pos->data.interpolation = INTERP_MODE_NOPERSPECTIVE;
      }

      /* b->cursor = nir_after_instr(nir_block_last_instr(nir_impl_last_block(nir_shader_get_entrypoint(b->shader)))); */
      b->cursor = nir_before_block(nir_start_block(nir_shader_get_entrypoint(b->shader)));

      /* TODO: vector when vectorizing */
      state->depth_feedback_src = nir_load_input(b, 1, 32, nir_imm_int(b, 0), .base = 0, .component = 2, .dest_type = nir_type_float32, .io_semantics.location = VARYING_SLOT_POS, .io_semantics.num_slots = 1);

      progress |= true;
   }

   return progress;
}

PUBLIC
bool rogue_nir_pfo(nir_shader *shader, rogue_build_ctx *ctx)
{
   assert(shader->info.stage == MESA_SHADER_FRAGMENT);

   bool progress = false;

   nir_builder b = nir_builder_create(nir_shader_get_entrypoint(shader));
   b.cursor =
      nir_before_block(nir_start_block(nir_shader_get_entrypoint(shader)));

   struct pfo_state state = {
      .fs_data = &ctx->stage_data.fs,
      .discard_cond_accum = nir_imm_false(&b),
      .depth_feedback_src = nir_undef(&b, 1, 32),
   };

   /* Lower depth feedback, inserting it if required. */
   progress |= lower_depth_feedback(&b, &state);

   /* Fragment outputs - lower to pack components. */
   progress |= nir_shader_lower_instructions(shader,
                                             is_frag_out,
                                             lower_frag_out,
                                             &state);

   /* Fragment outputs - move to the end. */
   progress |= sink_frag_outs(shader, &state);

   /* Lower ISP feedback (discards, depth, etc.) */
   progress |= lower_isp_fb(&b, &state);

#if 1
   /* Z-replicate */
   {
      struct rogue_fs_build_data *fs_data = &ctx->stage_data.fs;
      if (fs_data->z_replicate >= 0 && fs_data->depth_write) {
         /* TODO: test */
         assert(fs_data->z_replicate < fs_data->num_outputs);
         unsigned mrt_idx = fs_data->z_replicate;
         const struct usc_mrt_resource *mrt_resource =
            fs_data->outputs[mrt_idx].mrt_resource;

         assert(mrt_resource->type == USC_MRT_RESOURCE_TYPE_OUTPUT_REG);
         assert(mrt_resource->reg.offset == 0);

         nir_instr *last_instr = nir_block_last_instr(nir_impl_last_block(nir_shader_get_entrypoint(shader)));
         b.cursor = nir_after_instr(last_instr);

         nir_store_output(&b,
                          state.depth_feedback_src,
                          nir_imm_int(&b, 0),
                          .base = mrt_resource->reg.output_reg,
                          .write_mask = 1,
                          .component = 2, /* z */
                          .src_type = nir_type_invalid,
                             .io_semantics.location =
                                FRAG_RESULT_DATA0 + mrt_idx,
                             .io_semantics.num_slots = 1);
      }
   }
#endif

   /* Frag intrinsics. */
   progress |= nir_shader_lower_instructions(shader,
                                             is_frag_intrinsic,
                                             lower_frag_intrinsic,
                                             &ctx->stage_data.fs);

   /* printf("after pfo:\n"); */
   /* nir_print_shader(shader, stdout); */

   return progress;
}
