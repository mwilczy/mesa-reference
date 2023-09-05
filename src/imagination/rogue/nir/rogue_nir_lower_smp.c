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

static nir_def *lower_txs(nir_builder *b, unsigned num_components, unsigned info_base)
{
   assert(num_components <= 3);
   nir_def *size_comp[3]; /* TODO: define max dims or something */

   if (num_components > 0)
      size_comp[0] = nir_load_image_width_img(b, .info_base_img = info_base);

   if (num_components > 1)
      size_comp[1] = nir_load_image_height_img(b, .info_base_img = info_base);

   if (num_components > 2)
      size_comp[2] = nir_load_image_depth_img(b, .info_base_img = info_base);

   return nir_vec(b, size_comp, num_components);
}

static void lookup_base_regs(nir_tex_src *tex_state_src, nir_tex_src *smp_state_src, unsigned *tex_state_base, unsigned *smp_state_base, unsigned *info_base, enum pvr_stage_allocation pvr_stage, const rogue_build_ctx *ctx)
{
   const struct pvr_pipeline_layout *pipeline_layout = ctx->pipeline_layout;

   nir_binding combined_state_binding = nir_chase_binding(tex_state_src->src);
   assert(combined_state_binding.success);

   const struct pvr_descriptor_set_layout *combined_set_layout = pipeline_layout->set_layout[combined_state_binding.desc_set];
   assert(combined_set_layout);

   const struct pvr_descriptor_set_layout_binding *combined_binding_layout = pvr_get_descriptor_binding(combined_set_layout, combined_state_binding.binding);
   assert(combined_binding_layout);
   assert(combined_binding_layout->type == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER);

   struct pvr_descriptor_size_info combined_desc_size_info;
   pvr_descriptor_size_info_init(ctx->compiler->dev_info, pipeline_layout->robust_buffer_access, combined_binding_layout->type, &combined_desc_size_info);

   unsigned _tex_state_base = pvr_get_required_descriptor_primary_sh_reg(pipeline_layout, pvr_stage, combined_state_binding.desc_set, combined_binding_layout);

   if (tex_state_base)
      *tex_state_base = _tex_state_base;

   if (smp_state_base) {
      /* If sampler state provided as well, make sure we're dealing with a combined image sampler. */
      if (smp_state_src) {
         nir_binding smp_state_binding = nir_chase_binding(smp_state_src->src);
         assert(smp_state_binding.success);
         assert(combined_state_binding.desc_set == smp_state_binding.desc_set);
         assert(combined_state_binding.binding == smp_state_binding.binding);

         *smp_state_base = _tex_state_base + PVR_IMAGE_DESCRIPTOR_SIZE;
      } else {
         /* If not provided, point sampler needed. */
         abort();
      }
   }

   if (info_base)
      *info_base = pvr_get_sampler_descriptor_secondary_sh_reg(pipeline_layout, pvr_stage, combined_state_binding.desc_set, combined_binding_layout);
}

static nir_def *lower_smp(nir_builder *b, nir_instr *instr, void *cb_data)
{
   const rogue_build_ctx *ctx = cb_data;
   const struct pvr_pipeline_layout *pipeline_layout = ctx->pipeline_layout;

   nir_tex_instr *tex = nir_instr_as_tex(instr);

   enum glsl_sampler_dim sampler_dim = tex->sampler_dim;

   assert(nir_alu_type_get_type_size(tex->dest_type) == 32);

   assert(tex->is_shadow == tex->is_new_style_shadow);
   assert(!tex->is_sparse);

   assert(tex->component == 0); /* for tg4, ignore for now anyway */

   assert(!tex->array_is_lowered_cube);
   assert(!tex->is_gather_implicit_lod);
   /* assert(tex->tg4_offsets == ...); */ /* TODO */
   assert(!tex->texture_non_uniform);
   assert(!tex->sampler_non_uniform);

   assert(tex->texture_index == 0);
   assert(tex->sampler_index == 0);

   assert(tex->backend_flags == 0);

   nir_def *coords = NULL;
   nir_tex_src *tex_state_src = NULL;
   nir_tex_src *smp_state_src = NULL;
   nir_def *lod_replace = NULL;
   nir_def *lod_bias = NULL;
   nir_def *comparator = NULL;
   nir_def *offset = NULL;
   nir_def *proj = NULL;
   nir_def *ddx = NULL;
   nir_def *ddy = NULL;

   for (unsigned u = 0; u < tex->num_srcs; ++u) {
      switch (tex->src[u].src_type) {
      case nir_tex_src_coord:
         assert(!coords);
         coords = tex->src[u].src.ssa;
         break;

      case nir_tex_src_texture_deref:
         assert(!tex_state_src);
         tex_state_src = &tex->src[u];
         break;

      case nir_tex_src_sampler_deref:
         assert(!smp_state_src);
         smp_state_src = &tex->src[u];
         break;

      case nir_tex_src_lod:
         assert(!lod_replace);
         lod_replace = tex->src[u].src.ssa;
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

   if (tex->op == nir_texop_txs)
      return lower_txs(b, tex->def.num_components, info_base);

   assert(coords);
   assert(coords->num_components == tex->coord_components);

   nir_def *tex_addr_override = NULL;
   if (tex->is_array) {
      /* Separate out the array index component. */
      /* TODO: nir_f2u32_rte? */
      nir_def *array_idx = nir_convert_alu_types(b, 32, nir_channel(b, coords, tex->coord_components - 1), .src_type = nir_type_float32, .dest_type = nir_type_uint32, .rounding_mode = nir_rounding_mode_rtne);

      coords = nir_trim_vector(b, coords, tex->coord_components - 1);

      /* Clamp array index. */
      nir_def *image_array_maxidx = nir_load_image_array_maxidx_img(b, .info_base_img = info_base);
      array_idx = nir_bcsel(b, nir_ult(b, array_idx, image_array_maxidx), array_idx, image_array_maxidx);

      /* Calculate the new array address. */
      tex_addr_override = nir_iadd(b, nir_u2u64(b, nir_imul(b, array_idx, nir_load_image_array_stride_img(b, .info_base_img = info_base))), nir_u2u64(b, nir_load_image_array_base_addr_img(b, .info_base_img = info_base)));

#if 0
      /* Hmm... */
      if (!lod_replace && !lod_bias)
         lod_bias = nir_imm_int(b, 0);
#endif
   }

   unsigned chans = tex->def.num_components;

   unsigned data_comps = 0;
   nir_def *undef = nir_undef(b, 1, 32);
   nir_def *data[NIR_MAX_VEC_COMPONENTS] = { [0 ... (NIR_MAX_VEC_COMPONENTS - 1)] = undef };

   unsigned flags = 0;

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

   assert(data_comps <= NIR_MAX_VEC_COMPONENTS);

   nir_def *data_vec = nir_vec(b, data, ARRAY_SIZE(data));

   /* TODO: Only set this if texture/sampler type is **fixed**!
    * Need to query texture/sampler type.
    */
   if (nir_alu_type_get_base_type(tex->dest_type) == nir_type_float)
      flags |= BITFIELD_BIT(ROGUE_SMP_FLAG_FCNORM);

   nir_def *sample = nir_smp_img(b, chans, data_vec, .tex_state_base_img = tex_base, .smp_state_base_img = smp_base, .image_dim = sampler_dim, .flags = flags);

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

static bool is_smp(const nir_instr *instr, UNUSED const void *cb_data)
{
   if (instr->type != nir_instr_type_tex)
      return false;

#if 0
   /* TODO */
   nir_tex_instr *tex = nir_instr_as_tex(instr);
   if (nir_tex_instr_is_query(tex))
      return false;
#endif

   /* TODO: rest. */

   return true;
}

PUBLIC
bool rogue_nir_lower_smp(nir_shader *shader, rogue_build_ctx *ctx)
{
   return nir_shader_lower_instructions(shader, is_smp, lower_smp, ctx);
}
