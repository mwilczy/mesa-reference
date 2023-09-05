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
 * \file rogue_nir_lower_smp.c
 *
 * \brief Contains sampler/texture/image lowering passes.
 */

/* TODO: Probably want the binding -> base lookup part of this in rogue_nir_lower_vk_io.c */

#if 0
static unsigned tex_coord_components(const nir_tex_instr *tex)
{
   if (nir_tex_instr_src_index(tex, nir_tex_src_coord) < 0)
      return 0;

   unsigned coord_components = glsl_get_sampler_dim_coordinate_components(tex->sampler_dim);
   if (tex->is_array)
      coord_components++;

   return coord_components;
}
#endif

static nir_def *lower_txs(nir_builder *b, unsigned num_components, unsigned info_base, bool is_array)
{
   assert(num_components <= 3);
   nir_def *size_comp[3 + 1]; /* TODO: define max dims or something */ /* +1 for array */

   if (is_array)
      --num_components;

   unsigned c = 0;
   if (num_components > 0)
      size_comp[c++] = nir_load_image_width_img(b, .info_base_img = info_base);

   if (num_components > 1)
      size_comp[c++] = nir_load_image_height_img(b, .info_base_img = info_base);

   if (num_components > 2)
      size_comp[c++] = nir_load_image_depth_img(b, .info_base_img = info_base);

   if (is_array)
      size_comp[c++] = nir_iadd_imm(b, nir_load_image_array_maxidx_img(b, .info_base_img = info_base), 1);

   return nir_vec(b, size_comp, c);
}

static nir_def *lower_texture_samples(nir_builder *b, unsigned tex_base)
{
   nir_def *image_state_word_0_hi = nir_load_image_state_word_img(b, .tex_state_base_img = tex_base, .component = 1);
   nir_def *tex_samples_log2 = nir_ubitfield_extract(b, image_state_word_0_hi, nir_imm_int(b, 30), nir_imm_int(b, 2));
   nir_def *tex_samples = nir_ishl(b, nir_imm_int(b, 1), tex_samples_log2);

   return tex_samples;
}

static nir_def *lower_query_levels(nir_builder *b, unsigned tex_base)
{
   nir_def *image_state_word_1_lo = nir_load_image_state_word_img(b, .tex_state_base_img = tex_base, .component = 2);
   nir_def *query_levels = nir_ubitfield_extract(b, image_state_word_1_lo, nir_imm_int(b, 0), nir_imm_int(b, 4));

   return query_levels;
}

static void lookup_base_regs(nir_src *tex_state_src, nir_src *smp_state_src, unsigned *tex_state_base, unsigned *smp_state_base, unsigned *info_base, enum pvr_stage_allocation pvr_stage, const rogue_build_ctx *ctx)
{
   const struct pvr_pipeline_layout *pipeline_layout = ctx->pipeline_layout;

   nir_binding combined_state_binding = nir_chase_binding(*tex_state_src);
   assert(combined_state_binding.success);

   const struct pvr_descriptor_set_layout *combined_set_layout = pipeline_layout->set_layout[combined_state_binding.desc_set];
   assert(combined_set_layout);

   const struct pvr_descriptor_set_layout_binding *combined_binding_layout = pvr_get_descriptor_binding(combined_set_layout, combined_state_binding.binding);
   assert(combined_binding_layout);
   /* assert(combined_binding_layout->type == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER); */

   struct pvr_descriptor_size_info combined_desc_size_info;
   pvr_descriptor_size_info_init(ctx->compiler->dev_info, pipeline_layout->robust_buffer_access, combined_binding_layout->type, &combined_desc_size_info);

   /* Add the offset of the descriptor within the binding. */
   unsigned desc_elem = 0;
   if (combined_binding_layout->descriptor_count > 1) {
      assert(combined_state_binding.num_indices == 1);
      desc_elem = nir_src_as_uint(combined_state_binding.indices[0]);
   }

   unsigned _tex_state_base = pvr_get_required_descriptor_primary_sh_reg(pipeline_layout, pvr_stage, combined_state_binding.desc_set, combined_binding_layout);
   _tex_state_base += desc_elem * combined_desc_size_info.primary;

   if (tex_state_base)
      *tex_state_base = _tex_state_base;

   if (smp_state_base) {
      unsigned _smp_state_base = ROGUE_REG_UNUSED;
      if (smp_state_src) {
         nir_binding smp_state_binding = nir_chase_binding(*smp_state_src);
         assert(smp_state_binding.success);

         if (combined_binding_layout->type == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER) {
            _smp_state_base = _tex_state_base + PVR_IMAGE_DESCRIPTOR_SIZE;
         } else {
            /* TODO: commonise this */
            const struct pvr_descriptor_set_layout *smp_set_layout = pipeline_layout->set_layout[smp_state_binding.desc_set];
            assert(smp_set_layout);

            const struct pvr_descriptor_set_layout_binding *smp_binding_layout = pvr_get_descriptor_binding(smp_set_layout, smp_state_binding.binding);
            assert(smp_binding_layout);

            struct pvr_descriptor_size_info smp_desc_size_info;
            pvr_descriptor_size_info_init(ctx->compiler->dev_info, pipeline_layout->robust_buffer_access, smp_binding_layout->type, &smp_desc_size_info);

            /* Add the offset of the descriptor within the binding. */
            unsigned smp_desc_elem = 0;
            if (smp_binding_layout->descriptor_count > 1) {
               assert(smp_state_binding.num_indices == 1);
               smp_desc_elem = nir_src_as_uint(smp_state_binding.indices[0]);
            }

            _smp_state_base = pvr_get_required_descriptor_primary_sh_reg(pipeline_layout, pvr_stage, smp_state_binding.desc_set, smp_binding_layout);
            _smp_state_base += smp_desc_elem * smp_desc_size_info.primary;
         }

      } else {
         /* If not provided, point sampler needed. */
         assert(pipeline_layout->point_sampler_in_dwords_per_stage[pvr_stage] != ROGUE_REG_UNUSED);
         _smp_state_base = pipeline_layout->point_sampler_in_dwords_per_stage[pvr_stage];
      }

      *smp_state_base = _smp_state_base;
   }

   unsigned _info_base = pvr_get_sampler_descriptor_secondary_sh_reg(pipeline_layout, pvr_stage, combined_state_binding.desc_set, combined_binding_layout);
   _info_base += desc_elem * combined_desc_size_info.secondary;

   if (info_base)
      *info_base = _info_base;
}

static nir_def * chase_float_src(nir_builder *b, nir_def *val/* , bool round */)
{
   if (val->parent_instr->type == nir_instr_type_alu) {
      nir_alu_instr *alu = nir_instr_as_alu(val->parent_instr);
      if (alu->op == nir_op_f2i32) {
         nir_def *float_src = nir_ssa_for_alu_src(b, alu, 0);
         /* if (!round) */
            return float_src;

#if 0
         nir_def *rounded = nir_convert_alu_types(b, 32, float_src, .src_type = nir_type_float32, .dest_type = nir_type_int32, .rounding_mode = nir_rounding_mode_rtz);
         rounded = nir_convert_alu_types(b, 32, rounded, .src_type = nir_type_int32, .dest_type = nir_type_float32, .rounding_mode = nir_rounding_mode_rtz);
         return rounded;
#endif
      }
   }

   unreachable();
   /* return nir_i2f32(b, val); */
}

static const int8_t default_tg4_offsets[4][2] = {
   { 0, 1 },
   { 1, 1 },
   { 1, 0 },
   { 0, 0 },
};

static nir_def *lower_smp(nir_builder *b, nir_instr *instr, void *cb_data)
{
   const rogue_build_ctx *ctx = cb_data;

   nir_tex_instr *tex = nir_instr_as_tex(instr);

   enum glsl_sampler_dim sampler_dim = tex->sampler_dim;

   assert(nir_alu_type_get_type_size(tex->dest_type) == 32);

   assert(tex->is_shadow == tex->is_new_style_shadow);
   assert(!tex->is_sparse);

   /* TODO: tg4 */
   assert(tex->component == 0);
   assert(!memcmp(tex->tg4_offsets, default_tg4_offsets, sizeof(tex->tg4_offsets)));

   assert(!tex->array_is_lowered_cube);
   assert(!tex->is_gather_implicit_lod);
   assert(!tex->texture_non_uniform);
   assert(!tex->sampler_non_uniform);

   assert(tex->texture_index == 0);
   assert(tex->sampler_index == 0);

   assert(tex->backend_flags == 0);

   nir_def *coords = NULL;
   nir_src *tex_state_src = NULL;
   nir_src *smp_state_src = NULL;
   nir_def *lod_replace = NULL;
   nir_def *lod_bias = NULL;
   nir_def *comparator = NULL;
   nir_def *offset = NULL;
   nir_def *proj = NULL;
   nir_def *ddx = NULL;
   nir_def *ddy = NULL;
   nir_def *wrdata = NULL;

   unsigned flags = 0;

   bool int_comps = false;
   for (unsigned u = 0; u < tex->num_srcs; ++u) {
      switch (tex->src[u].src_type) {
      case nir_tex_src_coord:
         assert(!coords);
         coords = tex->src[u].src.ssa;
         if (nir_tex_instr_src_type(tex, u) != nir_type_float) {
#if 0
            coords = nir_i2f32(b, coords);
            flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_NNCOORDS);
#else
            /* TODO: get rid. this works.. but we don't want to use it. */
            flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_INTEGER);
            int_comps = true;
#endif
         }
         break;

      case nir_tex_src_texture_deref:
         assert(!tex_state_src);
         tex_state_src = &tex->src[u].src;
         break;

      case nir_tex_src_sampler_deref:
         assert(!smp_state_src);
         smp_state_src = &tex->src[u].src;
         break;

      case nir_tex_src_lod:
         assert(!lod_replace);
         lod_replace = tex->src[u].src.ssa;
#if 0
         if (nir_tex_instr_src_type(tex, u) != nir_type_float)
            lod_replace = nir_i2f32(b, lod_replace);
#endif
         break;

      case nir_tex_src_bias:
         assert(!lod_bias);
         lod_bias = tex->src[u].src.ssa;
         break;

      case nir_tex_src_comparator:
         assert(!comparator);
         comparator = tex->src[u].src.ssa;
         break;

      case nir_tex_src_offset:
         assert(!offset);
         offset = tex->src[u].src.ssa;
         break;

      case nir_tex_src_projector:
         assert(!proj);
         proj = tex->src[u].src.ssa;
         break;

      case nir_tex_src_ddx:
         assert(!ddx);
         ddx = tex->src[u].src.ssa;
         break;

      case nir_tex_src_ddy:
         assert(!ddy);
         ddy = tex->src[u].src.ssa;
         break;

      case nir_tex_src_wrdata:
         assert(!wrdata);
         wrdata = tex->src[u].src.ssa;
         break;

      default:
         unreachable("Unsupported tex src type.");
      }
   }

   assert(!(lod_replace && lod_bias));

   assert(tex_state_src);

   unsigned tex_base = ROGUE_REG_UNUSED;
   unsigned smp_base = ROGUE_REG_UNUSED;
   unsigned info_base = ROGUE_REG_UNUSED;

   enum pvr_stage_allocation pvr_stage = mesa_stage_to_pvr(b->shader->info.stage);
   lookup_base_regs(tex_state_src, smp_state_src, &tex_base, &smp_base, &info_base, pvr_stage, ctx);

   assert(tex_base != ROGUE_REG_UNUSED);
   assert(smp_base != ROGUE_REG_UNUSED);
   assert(info_base != ROGUE_REG_UNUSED);

   bool smp_info = false;
   if (nir_tex_instr_is_query(tex))
   switch (tex->op) {
   case nir_texop_txs:
      return lower_txs(b, tex->def.num_components, info_base, tex->is_array);

   case nir_texop_texture_samples:
      return lower_texture_samples(b, tex_base);

   case nir_texop_query_levels:
      return lower_query_levels(b, tex_base);

   case nir_texop_lod:
      smp_info = true;
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_INFO);
      break;

   default:
      unreachable("Unsupported texture query op.");
   }

   assert(coords);
   assert(coords->num_components == tex->coord_components);

   nir_def *tex_addr_override = NULL;
   if (tex->is_array && !nir_tex_instr_is_query(tex)) {
      /* Separate out the array index component. */
      /* TODO: nir_f2u32_rte? */

      nir_def *array_idx = nir_channel(b, coords, tex->coord_components - 1);

      if (!int_comps)
         array_idx = nir_convert_alu_types(b, 32, array_idx, .src_type = nir_type_float32, .dest_type = nir_type_uint32, .rounding_mode = nir_rounding_mode_rtne);

      coords = nir_trim_vector(b, coords, tex->coord_components - 1);

      /* Clamp array index. */
      nir_def *image_array_maxidx = nir_load_image_array_maxidx_img(b, .info_base_img = info_base);
      array_idx = nir_bcsel(b, nir_ult(b, array_idx, image_array_maxidx), array_idx, image_array_maxidx);

      /* Calculate the new array address. */
      tex_addr_override = nir_iadd(b, nir_u2u64(b, nir_imul(b, array_idx, nir_load_image_array_stride_img(b, .info_base_img = info_base))), nir_u2u64(b, nir_load_image_array_base_addr_img(b, .info_base_img = info_base)));
   }

   unsigned chans = tex->def.num_components;

   unsigned data_comps = 0;
   nir_def *undef = nir_undef(b, 1, 32);
   nir_def *data[NIR_MAX_VEC_COMPONENTS] = { [0 ... (NIR_MAX_VEC_COMPONENTS - 1)] = undef };

   /* Coordinates. */
   for (; data_comps < coords->num_components; ++data_comps)
      data[data_comps] = nir_channel(b, coords, data_comps);

   if (proj) {
      assert(proj->num_components == 1);
      data[data_comps++] = proj;
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_PROJ);
   }

   if (lod_replace) {
      data[data_comps++] = lod_replace;
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_LOD_REPLACE);
   } else if (lod_bias) {
      data[data_comps++] = lod_bias;
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_LOD_BIAS);
   }

   if (ddx) {
      assert(ddy);
      assert(ddx->num_components == ddy->num_components);
      assert(ddx->num_components == coords->num_components);

#if 0
      /* Special case, need to transform the gradients to the selected face. */
      if (sampler_dim == GLSL_SAMPLER_DIM_CUBE) {
         /* Calculate the face intersected by the coordinates. */
         /*
            if fabs(U) > fabs(S)
               if fabs(U) > fabs(V)
                  Face = (U >= 0) ? POSX : NEGX
               Else
                  Face = (V >= 0) ? POSY : NEGY
            Else
               if fabs(V) > fabs(S)
                  Face = (V >= 0) ? POSY : NEGY
               Else
                  Face = (S >= 0) ? POSZ : NEGZ
         */

         /* Transform the coordinates/gradients to that face. */
         /*
         static const MAP asMap[] =
         {
            [FACE_POS_X] = {.uMajorCoord = 0,	.bNegMajor = IMG_FALSE, .auFace = {2,	1}, .abNegFace = {IMG_TRUE,  IMG_TRUE}},
            [FACE_POS_Y] = {.uMajorCoord = 1,	.bNegMajor = IMG_FALSE, .auFace = {0,	2}, .abNegFace = {IMG_FALSE, IMG_FALSE}},
            [FACE_POS_Z] = {.uMajorCoord = 2,	.bNegMajor = IMG_FALSE, .auFace = {0,	1}, .abNegFace = {IMG_FALSE, IMG_TRUE}},
            [FACE_NEG_X] = {.uMajorCoord = 0,	.bNegMajor = IMG_TRUE,	.auFace = {2,	1}, .abNegFace = {IMG_FALSE, IMG_TRUE}},
            [FACE_NEG_Y] = {.uMajorCoord = 1,	.bNegMajor = IMG_TRUE,	.auFace = {0,	2}, .abNegFace = {IMG_FALSE, IMG_TRUE}},
            [FACE_NEG_Z] = {.uMajorCoord = 2,	.bNegMajor = IMG_TRUE,  .auFace = {0,	1}, .abNegFace = {IMG_TRUE,  IMG_TRUE}},
         };
         */

         /* Emit instructions to transform the coordinates/gradients to the target face. */
         /* sMajorSqr = sMajor * sMajor */
         /* sRcpMajorSqr = 1 / sMajorSqr */
         /* sRcpMajorSqrHalf = sRcpMajorSqr * 0.5 */
         /* for (IMG_UINT32 uDir = 0; uDir < UF_TEXTURE_NUM_SCREEN_DIRECTIONS; uDir++) */
            /* for (IMG_UINT32 uCoord = 0; uCoord < 2; uCoord++) */
               /* PCARG	psNewGrad = &psDest->asGrad[uCoord][uDir]; */
               /* sNewGrad = sMajor.sCoord * asFace[uCoord].asGrad[uDir] */
               /* sNewGrad = (-asFace[uCoord].sCoord) * sMajor.asGrad[uDir] + sNewGrad */
               /* sNewGrad = sNewGrad * sRcpMajorSqrHalf */

         /* Copy the coordinates/gradients to transform function's source arguments. */
         /* Copy the transformed gradients from the function's destinations. */
      }
#endif

      for (unsigned c = 0; c < ddx->num_components; ++c) {
         data[data_comps++] = nir_channel(b, ddx, c);
         data[data_comps++] = nir_channel(b, ddy, c);
      }

      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_GRADIENT);
   }

   if (tex_addr_override) {
      data[data_comps++] = nir_unpack_64_2x32_split_x(b, tex_addr_override);
      data[data_comps++] = nir_unpack_64_2x32_split_y(b, tex_addr_override);
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_TAO);
   }

   if (offset) {
      assert(offset->num_components == coords->num_components);

      nir_def* packed_offsets = nir_imm_int(b, 0);

      unsigned packed_offset_start[] = { 0, 6, 12 };
      unsigned packed_offset_size[] = { 6, 6, 4 };

      for (unsigned c = 0; c < offset->num_components; ++c) {
         packed_offsets = nir_bitfield_insert(b,
                                               packed_offsets,
                                               nir_channel(b, offset, c),
                                               nir_imm_int(b, packed_offset_start[c]),
                                               nir_imm_int(b, packed_offset_size[c]));
      }

      data[data_comps++] = packed_offsets;
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_SOO);
   }

   if (wrdata) {
      data[data_comps++] = nir_channel(b, wrdata, 0);
      data[data_comps++] = nir_channel(b, wrdata, 1);
      data[data_comps++] = nir_channel(b, wrdata, 2);
      data[data_comps++] = nir_channel(b, wrdata, 3);

      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_WRT);
   }

   assert(data_comps <= NIR_MAX_VEC_COMPONENTS);

   nir_def *data_vec = nir_vec(b, data, ARRAY_SIZE(data));

   /* TODO: Only set this if texture/sampler type is **fixed**!
    * Need to query texture/sampler type.
    */
   if (nir_alu_type_get_base_type(tex->dest_type) == nir_type_float)
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_FCNORM);

   nir_def *sample = nir_smp_img(b, smp_info ? 16 : chans, data_vec, .tex_state_base_img = tex_base, .smp_state_base_img = smp_base, .image_dim = sampler_dim, .flags = flags);

   if (smp_info) {
      assert(!tex->is_shadow);
      assert(tex->op == nir_texop_lod);

      /*
      loadlod:
          [0] = (255.0f * Lod_dval_post_clamp) + (0.996094f * Tfrac_post_clamp)
          [1] = ((abs(fdsx(coord)) + abs(fdsy(coord)) != 0.0f) ? (255.0f * Lod_dval_pre_clamp - 128.0f) : -128.0f) + (0.996094f * Tfrac_pre_clamp)
       */

      nir_def *coord_deltas = nir_imm_int(b, 0);
      for (unsigned c = 0; c < coords->num_components; ++c) {
         nir_def *coord = nir_channel(b, coords, c);
         coord_deltas = nir_fadd(b, coord_deltas, nir_fadd(b, nir_fabs(b, nir_fddx(b, coord)), nir_fabs(b, nir_fddy(b, coord))));
      }

      nir_def *f255 = nir_imm_float(b, 255.0f);
      nir_def *f256 = nir_imm_float(b, 256.0f);
      nir_def *fneg_128 = nir_imm_float(b, -128.0f);

      nir_def *lod_post_clamp = nir_channel(b, sample, ROGUE_SMP_INFO_LOD_POSTCLAMP);
      nir_def *lod_pre_clamp = nir_channel(b, sample, ROGUE_SMP_INFO_LOD_PRECLAMP);
      nir_def *tfrac_post_clamp = nir_channel(b, sample, ROGUE_SMP_INFO_TFRAC_POSTCLAMP);
      nir_def *tfrac_pre_clamp = nir_channel(b, sample, ROGUE_SMP_INFO_TFRAC_PRECLAMP);

      assert(tex->def.num_components == 2);

      nir_def *lod0 = nir_fmul(b, f255, lod_post_clamp);
      lod0 = nir_fadd(b, lod0, nir_fdiv(b, nir_fmul(b, f255, tfrac_post_clamp), f256));

      nir_def *lod1 = nir_fadd(b, nir_fmul(b, f255, lod_pre_clamp), fneg_128);
      lod1 = nir_fcsel(b, coord_deltas, lod1, fneg_128);
      lod1 = nir_fadd(b, lod1, nir_fdiv(b, nir_fmul(b, f255, tfrac_pre_clamp), f256));

      return nir_vec2(b, lod0, lod1);
   }

   if (tex->is_shadow) {
      assert(chans == 1);

      /*
       * An important note on shadow samplers. The comparison value is still part of the texture coordinate,
       * stored in the next to last component. Because it is part of the texture coordinate, it is also
       * divided by the last coordinate before the comparison. This means that you must pre-multiply
       * the actual value by the projection value.
       * (=== divide the comparator value by the projection value)
       */
      if (proj)
         comparator = nir_fdiv(b, comparator, proj);

      sample = nir_bcsel(b, nir_flt(b, comparator, sample), nir_imm_float(b, 1.0f), nir_imm_float(b, 0.0f));
   }

   return sample;
}

/* TODO: rename all to _tex? */
static bool is_smp(const nir_instr *instr, UNUSED const void *cb_data)
{
   return instr->type == nir_instr_type_tex;
}

PUBLIC
bool rogue_nir_lower_smp(nir_shader *shader, rogue_build_ctx *ctx)
{
   return nir_shader_lower_instructions(shader, is_smp, lower_smp, ctx);
}
