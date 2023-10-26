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

#include "compiler/spirv/nir_spirv.h"
#include "nir/nir.h"
#include "nir/nir_builder.h"
#include "nir/nir_legacy.h"
#include "rogue.h"
#include "util/macros.h"

#include <stdbool.h>

/**
 * \file rogue_nir.c
 *
 * \brief Contains SPIR-V and NIR-specific functions.
 */

/**
 * \brief SPIR-V to NIR compilation options.
 */
static const struct spirv_to_nir_options spirv_options = {
   .environment = NIR_SPIRV_VULKAN,

   /* TODO: set these from the driver. */
   .caps = {
      .device_group = true,
      /* .int16 = true, */
      /* .int64 = true, */
      /* .int8 = true, */
      /* .storage_16bit = true, */
      /* .storage_8bit = true, */
      /* .float32_atomic_add = true, */
      /* .float32_atomic_min_max = true, */
   },

   .ubo_addr_format = nir_address_format_32bit_index_offset,
   .ssbo_addr_format = nir_address_format_32bit_index_offset,

   .phys_ssbo_addr_format = nir_address_format_64bit_global,
   .push_const_addr_format = nir_address_format_32bit_offset,
   .shared_addr_format = nir_address_format_32bit_offset,

   .temp_addr_format = nir_address_format_32bit_offset,
   /* .constant_addr_format = nir_address_format_64bit_global, */
};

typedef struct rogue_varying_order_info {
   bool f16;
   enum glsl_interp_mode mode;
   unsigned index;
} rogue_varying_order_info;

inline static rogue_varying_order_info
rogue_varying_order_info_from_var(const nir_variable *var)
{
   return (rogue_varying_order_info){
      .f16 = glsl_type_is_16bit(var->type),
      .mode = var->data.interpolation != INTERP_MODE_NONE
                 ? var->data.interpolation
                 : INTERP_MODE_SMOOTH,
      .index = rogue_from_gl_varying_loc(var->data.location) * 4 +
               var->data.location_frac,
   };
}

static bool rogue_varying_order_before(const nir_variable *var,
                                       const nir_variable *new_var)
{
   rogue_varying_order_info var_info = rogue_varying_order_info_from_var(var);
   rogue_varying_order_info new_var_info =
      rogue_varying_order_info_from_var(new_var);

   if (new_var_info.f16 < var_info.f16)
      return true;

   if (new_var_info.mode < var_info.mode)
      return true;

   if (new_var_info.index < var_info.index)
      return true;

   return false;
}

static bool rogue_nonvarying_order_before(const nir_variable *var,
                                          const nir_variable *new_var)
{
   if (new_var->data.per_primitive < var->data.per_primitive)
      return true;

   if (new_var->data.per_primitive == var->data.per_primitive) {
      if (var->data.location > new_var->data.location)
         return true;

      if (var->data.location == new_var->data.location &&
          var->data.location_frac > new_var->data.location_frac)
         return true;
   }

   return false;
}

static void rogue_sort_varying_cb(struct exec_list *var_list,
                                  nir_variable *new_var,
                                  nir_variable_mode mode,
                                  gl_shader_stage stage)
{
   bool is_loc_varying =
      (stage == MESA_SHADER_VERTEX && mode == nir_var_shader_out) ||
      (stage == MESA_SHADER_FRAGMENT && mode == nir_var_shader_in);
   bool is_varying_new_var =
      rogue_from_gl_varying_loc(new_var->data.location) != ~0;

   nir_foreach_variable_in_list (var, var_list) {
      bool is_varying_var = rogue_from_gl_varying_loc(var->data.location) != ~0;
      bool is_varying_vars = is_loc_varying && is_varying_new_var &&
                             is_varying_var;
      bool order_before = is_varying_vars
                             ? rogue_varying_order_before(var, new_var)
                             : rogue_nonvarying_order_before(var, new_var);

      if (order_before) {
         exec_node_insert_node_before(&var->node, &new_var->node);
         return;
      }
   }
   exec_list_push_tail(var_list, &new_var->node);
}

static const nir_shader_compiler_options nir_options = {
   .lower_device_index_to_zero = true,
   .lower_fdiv = true,
   .fuse_ffma32 = true,
   .lower_flrp16 = true,
   .lower_flrp32 = true,
   .lower_flrp64 = true,
   .lower_fpow = true,
   .lower_fsqrt = true,
   .lower_fmod = true,
   .lower_ftrunc = true,
   .lower_insert_byte = true,
   .lower_insert_word = true,
   .lower_ifind_msb = true,
   .lower_find_lsb = true,
   .lower_uadd_carry = true,
   .lower_usub_borrow = true,
   .lower_isign = true,
   .lower_fsign = true,
   .lower_ffract = true,
   .lower_rotate = true, /* TODO: add nir option to convert ror to rol then
                            enable this. */
   .has_fused_comp_and_csel = true,
   /* TODO: Remove these and instead merge components in the backend instead. */
   .has_fsub = true,
   .has_isub = true,
   .lower_fsat = true,
   .lower_fceil = true,
   .lower_ldexp = true,
   .lower_interpolate_at = true,
   .support_8bit_alu = true,
   .support_16bit_alu = true,
#if 0
   .max_unroll_iterations = 1,
#else
   .max_unroll_iterations = 16,
   /* .force_indirect_unrolling_sampler = true, */
   /* .force_indirect_unrolling = nir_var_all, */
#endif
   .sort_varying_cb = rogue_sort_varying_cb,
   /* TODO: exclude the remaining native int64 ops we actually support. */
   .lower_int64_options = ~0 & ~nir_lower_iadd64 & ~nir_lower_iabs64 &
                          ~nir_lower_ineg64,
};

static int rogue_glsl_type_size(const struct glsl_type *type, bool bindless)
{
   return glsl_count_attribute_slots(type, false);
}

static void rogue_nir_opt_loop(struct rogue_build_ctx *ctx, nir_shader *nir)
{
   bool progress;

   do {
      progress = false;

      NIR_PASS(progress, nir, nir_opt_combine_stores, nir_var_all);
      NIR_PASS(progress,
               nir,
               nir_remove_dead_variables,
               (nir_variable_mode)(nir_var_function_temp | nir_var_shader_temp),
               NULL);

      NIR_PASS(progress, nir, nir_lower_var_copies);
      NIR_PASS(progress, nir, nir_lower_vars_to_ssa);

      NIR_PASS(progress, nir, nir_opt_copy_prop_vars);
      NIR_PASS(progress, nir, nir_opt_dead_write_vars);

      NIR_PASS(progress, nir, nir_copy_prop);
      NIR_PASS(progress, nir, nir_lower_phis_to_scalar, true);
      NIR_PASS(progress, nir, nir_opt_dce);
      NIR_PASS(progress, nir, nir_opt_dead_cf);
      NIR_PASS(progress, nir, nir_opt_cse);

      /* if (!ROGUE_DEBUG(SKIP_CF_OPTS)) */
         NIR_PASS(progress, nir, nir_opt_peephole_select, 64, false, true);

      NIR_PASS(progress, nir, nir_lower_int64);
      NIR_PASS(progress, nir, nir_lower_alu);
      NIR_PASS(progress, nir, nir_lower_pack);

      NIR_PASS(progress, nir, rogue_nir_lower_fquantize2f16);

      NIR_PASS(progress, nir, nir_opt_algebraic);
      NIR_PASS(progress, nir, rogue_nir_lower_fround_even);
      NIR_PASS(progress, nir, nir_opt_constant_folding);

      NIR_PASS(progress, nir, nir_opt_remove_phis);

      bool trivial_continues = false;
      NIR_PASS(trivial_continues, nir, nir_opt_trivial_continues);
      if (trivial_continues) {
         progress |= true;
         NIR_PASS(progress, nir, nir_copy_prop);
         NIR_PASS(progress, nir, nir_opt_dce);
         NIR_PASS(progress, nir, nir_opt_remove_phis);
      }

      if (!ROGUE_DEBUG(SKIP_CF_OPTS))
         NIR_PASS(progress, nir, nir_opt_if, nir_opt_if_aggressive_last_continue | nir_opt_if_optimize_phi_true_false);

      NIR_PASS(progress, nir, nir_opt_dead_cf);
      NIR_PASS(progress, nir, nir_opt_conditional_discard);
      NIR_PASS(progress, nir, nir_opt_remove_phis);
      NIR_PASS(progress, nir, nir_opt_cse);

      NIR_PASS(progress, nir, nir_opt_undef);
      NIR_PASS(progress, nir, nir_lower_undef_to_zero);

      NIR_PASS(progress, nir, nir_opt_deref);
      NIR_PASS(progress, nir, nir_lower_alu_to_scalar, NULL, NULL);

      if (!ROGUE_DEBUG(SKIP_CF_OPTS))
         NIR_PASS(progress, nir, nir_opt_loop_unroll);
   } while (progress);
}

static void
shared_var_info(const struct glsl_type *type, unsigned *size, unsigned *align)
{
   assert(glsl_type_is_vector_or_scalar(type));

   uint32_t comp_size =
      glsl_type_is_boolean(type) ? 4 : glsl_get_bit_size(type) / 8;
   unsigned length = glsl_get_vector_elements(type);
   *size = comp_size * length, *align = comp_size;
}

static bool
has_side_effects(struct nir_builder *b, nir_instr *instr, void *data)
{
   bool *_has_side_effects = (bool *)data;

   if (instr->type != nir_instr_type_intrinsic)
      return false;

   nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
   *_has_side_effects |= nir_intrinsic_writes_external_memory(intr);

   return false;
}

static bool rogue_nir_has_side_effects(nir_shader *shader)
{
   bool _has_side_effects = false;

   nir_shader_instructions_pass(shader, has_side_effects, nir_metadata_block_index | nir_metadata_dominance, &_has_side_effects);

   return _has_side_effects;
}

static void _rogue_nir_opt_loop(struct rogue_build_ctx *ctx, nir_shader *nir);

/**
 * \brief Applies optimizations and passes required to lower the NIR shader into
 * a form suitable for lowering to Rogue IR.
 *
 * \param[in] ctx Shared multi-stage build context.
 * \param[in] shader Rogue shader.
 * \param[in] stage Shader stage.
 */
static void
rogue_nir_passes(rogue_build_ctx *ctx, nir_shader *nir, gl_shader_stage stage)
{
   bool progress;

#if !defined(NDEBUG)
   bool nir_debug_print_shader_prev = nir_debug_print_shader[nir->info.stage];
   /* if (nir->info.stage == MESA_SHADER_FRAGMENT) */
      nir_debug_print_shader[nir->info.stage] = ROGUE_DEBUG(NIR_PASSES);
#endif /* !defined(NDEBUG) */

   nir_validate_shader(nir, "after spirv_to_nir");

   if (nir->info.stage == MESA_SHADER_VERTEX)
      ctx->stage_data.vs.side_effects = rogue_nir_has_side_effects(nir);
   else if (nir->info.stage == MESA_SHADER_FRAGMENT)
      ctx->stage_data.fs.side_effects = rogue_nir_has_side_effects(nir);

#if 0
   NIR_PASS_V(nir, nir_lower_vars_to_explicit_types, nir_var_function_temp, glsl_get_natural_size_align_bytes);
   NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_function_temp, spirv_options.temp_addr_format); /* TODO: need pass to DWORD addressing on scratch */
   NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_function_temp, NULL);
#endif

#if 0
   if (nir->info.stage == MESA_SHADER_COMPUTE)
      NIR_PASS_V(nir, rogue_nir_compute_instance_check);

   const struct nir_lower_sysvals_to_varyings_options sysvals_to_varyings = {
      .point_coord = true,
      .frag_coord = true,
   };
   NIR_PASS_V(nir, nir_lower_sysvals_to_varyings, &sysvals_to_varyings);

   /* Inlining. */
   NIR_PASS_V(nir, nir_lower_returns);
   NIR_PASS_V(nir, nir_inline_functions);
   NIR_PASS_V(nir, nir_copy_prop);
   NIR_PASS_V(nir, nir_opt_deref);
   nir_remove_non_entrypoints(nir);

   NIR_PASS_V(nir, nir_lower_variable_initializers, nir_var_shader_out);
   NIR_PASS_V(nir, nir_lower_variable_initializers, ~0);
#endif

   if (stage == MESA_SHADER_VERTEX) {
      NIR_PASS_V(nir, nir_lower_clip_cull_distance_arrays);
      NIR_PASS_V(nir,
                 nir_lower_point_size,
                 PVR_POINT_SIZE_RANGE_MIN,
                 PVR_POINT_SIZE_RANGE_MAX);
   }

   NIR_PASS_V(nir, nir_split_var_copies);
   NIR_PASS_V(nir, nir_split_per_member_structs);
   /* NIR_PASS_V(nir, nir_split_struct_vars, nir_var_function_temp); */

#if 0
   if (nir->info.stage == MESA_SHADER_FRAGMENT)
      NIR_PASS_V(nir, rogue_nir_lower_input_attachments, ctx);
#endif

   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      /* Lower fb input attachments. */
      NIR_PASS_V(nir, rogue_nir_lower_input_attachments, ctx);

      /* Lower tex input attachments. */
      NIR_PASS_V(nir, nir_lower_input_attachments, &(nir_input_attachment_options){ .use_layer_id_sysval = true, });
   }

   NIR_PASS_V(nir,
              nir_remove_dead_variables,
              nir_var_shader_in | nir_var_shader_out | nir_var_system_value,
              NULL);

   NIR_PASS_V(nir, nir_lower_global_vars_to_local);
   NIR_PASS_V(nir, nir_lower_vars_to_ssa);

   /* TODO */
   /* NIR_PASS_V(nir, nir_lower_locals_to_regs, 1); */

   NIR_PASS_V(nir, nir_opt_remove_phis);

   NIR_PASS_V(nir,
              nir_lower_io_to_temporaries,
              nir_shader_get_entrypoint(nir),
              true,
              true);

   /* NIR_PASS_V(nir, nir_lower_indirect_derefs, nir_var_function_temp, ~0); */

   NIR_PASS_V(nir, nir_split_var_copies);
   NIR_PASS_V(nir, nir_lower_global_vars_to_local);
   NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_function_temp, NULL);

   NIR_PASS_V(nir, nir_lower_var_copies);

   NIR_PASS_V(nir, nir_opt_constant_folding);
   /* NIR_PASS_V(nir, nir_lower_system_values); */

   /* Generate preamble shader. */
   if (ROGUE_DEBUG(PREAMBLE))
      NIR_PASS_V(nir, rogue_nir_opt_preamble, ctx);

   _rogue_nir_opt_loop(ctx, nir);

   /* Replace references to I/O variables with intrinsics. */
   NIR_PASS_V(nir,
              nir_lower_io,
              nir_var_shader_in | nir_var_shader_out,
              rogue_glsl_type_size,
              (nir_lower_io_options)0);

   /* Clean up deref_vars. */
   NIR_PASS_V(nir, nir_opt_dce);
   NIR_PASS_V(nir, nir_opt_constant_folding);
   NIR_PASS_V(nir,
              nir_io_add_const_offset_to_base,
              nir_var_shader_in | nir_var_shader_out);

   /* Load inputs to scalars (single registers later). */
   /* TODO: Fitrp can process multiple frag inputs at once, scalarise I/O. */
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_shader_in, NULL, NULL);

   /* Optimize GL access qualifiers. */
   const nir_opt_access_options opt_access_options = {
      .is_vulkan = true,
   };
   NIR_PASS_V(nir, nir_opt_access, &opt_access_options);

   NIR_PASS_V(nir, nir_opt_barrier_modes);

   /* Apply PVO code to the vertex shader input. */
   if (nir->info.stage == MESA_SHADER_VERTEX) {
      NIR_PASS_V(nir, rogue_nir_pvo, ctx);
      NIR_PASS_V(nir, nir_opt_dce);
   }

   /* Apply PFO code to the fragment shader output. */
   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      NIR_PASS_V(nir, rogue_nir_lower_blend, ctx);
      NIR_PASS_V(nir, rogue_nir_lower_blend_consts, ctx);
      NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_shader_out, NULL, NULL);
      NIR_PASS_V(nir, nir_opt_conditional_discard);
      NIR_PASS_V(nir, rogue_nir_pfo, ctx);
      /* NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_shader_out); */
      NIR_PASS_V(nir, nir_opt_dce);
   }

   /* Load outputs to scalars (single registers later). */
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_shader_out, NULL, NULL);

   /* Lower load_consts to scalars. */
   NIR_PASS_V(nir, nir_lower_load_const_to_scalar);

   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      unsigned samples = ctx->stage_data.fs.rasterization_samples;
      if (samples < 2)
         NIR_PASS_V(nir, nir_lower_single_sampled);

      NIR_PASS_V(nir, rogue_nir_lower_interpolation, samples);
   }

   /* Lower ALU operations to scalars. */
   NIR_PASS_V(nir, nir_lower_alu_to_scalar, NULL, NULL);
   NIR_PASS(progress, nir, nir_lower_load_const_to_scalar);

   /* TODO: does always_precise need to be true? */
   NIR_PASS_V(nir, nir_lower_flrp, 16 | 32 | 64, true);

   /* NIR_PASS_V(nir, nir_lower_memcpy); */

   /* Additional I/O lowering. */
   NIR_PASS_V(nir,
              nir_lower_explicit_io,
              nir_var_mem_push_const,
              spirv_options.push_const_addr_format);

   NIR_PASS_V(nir,
              nir_lower_explicit_io,
              nir_var_mem_ubo,
              spirv_options.ubo_addr_format);
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_mem_ubo, NULL, NULL);

   NIR_PASS_V(nir,
              nir_lower_explicit_io,
              nir_var_mem_ssbo,
              spirv_options.ssbo_addr_format);
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_mem_ssbo, NULL, NULL);

   NIR_PASS_V(nir, nir_opt_sink, nir_move_load_ubo | nir_move_load_ssbo | nir_move_load_input);
   NIR_PASS_V(nir, nir_opt_move, nir_move_load_ubo | nir_move_load_ssbo | nir_move_load_input);

   bool robust_buffer_access = ctx->pipeline_layout->robust_buffer_access;
   NIR_PASS_V(nir, nir_lower_robust_access, &(nir_lower_robust_access_options) {
         .lower_ubo = robust_buffer_access,
         .lower_ssbo = robust_buffer_access,
         });

   NIR_PASS_V(nir, rogue_nir_lower_bos);
   NIR_PASS_V(nir, nir_opt_dce);

   if (!nir->info.shared_memory_explicit_layout)
      NIR_PASS_V(nir,
                 nir_lower_vars_to_explicit_types,
                 nir_var_mem_shared,
                 shared_var_info);

   NIR_PASS_V(nir,
              nir_lower_explicit_io,
              nir_var_mem_shared,
              spirv_options.shared_addr_format);
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_mem_shared, NULL, NULL);

#if 0
   NIR_PASS_V(nir,
              nir_lower_explicit_io,
              /* nir_var_shader_temp | */ nir_var_function_temp,
              spirv_options.temp_addr_format);
   NIR_PASS_V(nir, nir_lower_io_to_scalar, /* nir_var_shader_temp | */ nir_var_function_temp, NULL, NULL);
#endif

#if 0
   NIR_PASS_V(nir,
              nir_lower_explicit_io,
              nir_var_mem_constant,
              spirv_options.constant_addr_format);
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_mem_constant, NULL, NULL);
#endif

#if 1
   if (!nir->info.internal) {
      /* NIR_PASS_V(nir, nir_lower_vars_to_explicit_types, nir_var_function_temp, function_temp_type_info); */
      NIR_PASS_V(nir, nir_lower_vars_to_explicit_types, nir_var_function_temp, glsl_get_natural_size_align_bytes);
      /* NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_function_temp, NULL, NULL); */
      /* NIR_PASS_V(nir, nir_lower_explicit_io, nir_var_function_temp, spirv_options.temp_addr_format); /1* TODO: need pass to DWORD addressing on scratch *1/ */
   NIR_PASS(progress,
            nir,
            nir_lower_vars_to_scratch,
            nir_var_function_temp,
            /* 256, */
            /* 16, */
            4,
            glsl_get_natural_size_align_bytes);
      NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_function_temp, NULL);

      NIR_PASS_V(nir, rogue_nir_lower_scratch);
   }
#endif

   NIR_PASS_V(nir, nir_lower_indirect_derefs, nir_var_function_temp, UINT32_MAX);

   NIR_PASS_V(nir, rogue_nir_lower_vk_io, ctx);

#if 0
   if (nir->info.stage == MESA_SHADER_COMPUTE)
      NIR_PASS_V(nir,
                 nir_lower_compute_system_values,
                 &(nir_lower_compute_system_values_options){
                    .lower_cs_local_id_to_index = true,
                 });
#endif

   /* NIR_PASS_V(nir, rogue_nir_lower_compute_intrinsics); */

   NIR_PASS_V(nir, rogue_nir_lower_io, ctx, false);

   /* TODO: should really only need to do this once, and also split up lowering
    * i/o and sysvals (and rewrite to use callback functions) need
    * nir_lower_compute_system_values to lower global invocation id to workgroup
    * id, but to not eliminate the local invocation id by making it a const 0
    * also need to check if that's actually what vtx0 is...
    */
   NIR_PASS_V(nir, rogue_nir_lower_io, ctx, true);

   /* Scalarise any resulting load/store_globals. */
   NIR_PASS_V(nir, nir_lower_io_to_scalar, nir_var_mem_global, NULL, NULL);

   NIR_PASS_V(nir, nir_lower_vars_to_ssa);

   NIR_PASS_V(nir, nir_propagate_invariant, false);

   /* Lower samplers. */
   NIR_PASS_V(nir, nir_opt_dce);
   NIR_PASS_V(nir, nir_opt_deref);
   NIR_PASS_V(nir, nir_lower_image_atomics_to_global);
   NIR_PASS_V(nir, rogue_nir_lower_images_to_tex);
   /* NIR_PASS_V(nir, nir_lower_readonly_images_to_tex, true); */
   /* NIR_PASS_V(nir, rogue_nir_lower_tex, ctx); */
   /* NIR_PASS_V(nir, nir_lower_samplers); */
   NIR_PASS_V(nir, nir_lower_tex, &(nir_lower_tex_options){ .lower_txd_cube_map = true, });
   NIR_PASS_V(nir, nir_lower_tex, &(nir_lower_tex_options){ .lower_txs_lod = true, });
   NIR_PASS_V(nir, rogue_nir_lower_smp, ctx);

   /* Lower atomic ops that aren't supported in hardware. */
   NIR_PASS_V(nir, rogue_nir_lower_atomics);

   NIR_PASS_V(nir, nir_opt_combine_barriers, NULL, NULL);
   NIR_PASS_V(nir, rogue_nir_lower_barriers);

   rogue_nir_opt_loop(ctx, nir);

   nir_lower_idiv_options idiv_options = {
      .allow_fp16 = false,
   };

   NIR_PASS_V(nir, nir_opt_idiv_const, 8);
   NIR_PASS_V(nir, nir_lower_idiv, &idiv_options);
   /* NIR_PASS_V(nir, nir_lower_frexp); */
   NIR_PASS_V(nir, nir_lower_alu_to_scalar, NULL, NULL);
   NIR_PASS_V(nir, nir_lower_load_const_to_scalar);

   rogue_nir_opt_loop(ctx, nir);

   NIR_PASS_V(nir, nir_remove_dead_variables, nir_var_function_temp, NULL);

   /* Late algebraic opts. */
   do {
      progress = false;

      NIR_PASS(progress, nir, rogue_nir_opt_fold_packs);
      NIR_PASS(progress, nir, rogue_nir_opt_algebraic_late);
      NIR_PASS(progress, nir, nir_opt_algebraic_late);
      /*
       * NIR_PASS_V(nir, nir_lower_to_source_mods, nir_lower_all_source_mods);
       */
      NIR_PASS_V(nir, nir_opt_constant_folding);
      NIR_PASS_V(nir, nir_copy_prop);
      NIR_PASS_V(nir, nir_opt_dce);
      NIR_PASS_V(nir, nir_opt_cse);
   } while (progress);

   NIR_PASS_V(nir, nir_lower_bool_to_int32);
   NIR_PASS_V(nir, rogue_nir_lower_alu_conversion_to_intrinsic);
   NIR_PASS_V(nir, nir_opt_constant_folding);

   NIR_PASS_V(nir, nir_lower_load_const_to_scalar);

   /* Remove unused constant registers. */
   NIR_PASS_V(nir, nir_opt_dce);

   /* TODO: Clean up duplicates and eventually remove this. */
   /* TODO: if the swizzle is e.g. xxxx, this will work out of the box with
    * rpt=1! */
   /* NIR_PASS_V(nir, rogue_nir_expand_swizzles_to_vec); */

#if 1
   /* NIR_PASS_V(nir, nir_opt_sink, nir_move_const_undef | nir_move_copies); */
   NIR_PASS_V(nir, nir_opt_move, nir_move_const_undef | nir_move_copies);
#endif

   /* Out of SSA pass. */
   NIR_PASS_V(nir, nir_convert_from_ssa, true);

   NIR_PASS_V(nir, nir_opt_dce);

   /* TODO: Re-enable scheduling after register pressure tweaks. */
#if 0
	/* Instruction scheduling. */
	struct nir_schedule_options schedule_options = {
		.threshold = ROGUE_MAX_REG_TEMP / 2,
	};
	NIR_PASS_V(nir, nir_schedule, &schedule_options);
#endif

#if 1
   progress = false;
   NIR_PASS(progress, nir, nir_opt_rematerialize_compares);
   if (progress)
      NIR_PASS_V(nir, nir_opt_dce);
#endif

   /* We're not fusing any source mods in NIR, so calling this pass
    * instead of nir_legacy_trivialize.
    */
   NIR_PASS_V(nir, nir_trivialize_registers);

   /* Assign I/O locations. */
   nir_assign_io_var_locations(nir,
                               nir_var_shader_in,
                               &nir->num_inputs,
                               nir->info.stage);
   nir_assign_io_var_locations(nir,
                               nir_var_shader_out,
                               &nir->num_outputs,
                               nir->info.stage);

   /* Renumber SSA defs. */
   nir_index_ssa_defs(nir_shader_get_entrypoint(nir));

   /* Gather info into nir shader struct. */
   nir_shader_gather_info(nir, nir_shader_get_entrypoint(nir));

   /* Clean-up after passes. */
   if (!nir->info.internal)
      nir_sweep(nir);

   nir_validate_shader(nir, "after passes");
   if (ROGUE_DEBUG(NIR)) {
      fputs("after passes\n", stdout);
      nir_print_shader(nir, stdout);
   }

#if !defined(NDEBUG)
   nir_debug_print_shader[nir->info.stage] = nir_debug_print_shader_prev;
#endif /* !defined(NDEBUG) */
}

/* TODO: commonise. */
static void rogue_collect_early_vs_build_data(rogue_build_ctx *ctx,
                                              nir_shader *nir)
{
   const struct shader_info *info = &nir->info;
   struct rogue_vs_build_data *vs_data = &ctx->stage_data.vs;
   rogue_vertex_special_vars *special_vars = &vs_data->special_vars;

   nir_foreach_function (func, nir) {
      nir_foreach_block (block, func->impl) {
         nir_foreach_instr (instr, block) {
            switch (instr->type) {
            case nir_instr_type_intrinsic:
               switch (nir_instr_as_intrinsic(instr)->intrinsic) {
               case nir_intrinsic_global_atomic:
               case nir_intrinsic_shared_atomic_img:
               case nir_intrinsic_global_atomic_swap:
               case nir_intrinsic_shared_atomic_swap_img:
                  vs_data->has.atomic_ops = true;
                  break;

               case nir_intrinsic_load_vertex_id:
                  special_vars->has.vertex_id = true;
                  break;

               case nir_intrinsic_load_instance_id:
                  special_vars->has.instance_id = true;
                  break;

               case nir_intrinsic_load_base_instance:
                  special_vars->has.base_instance = true;
                  break;

               case nir_intrinsic_load_base_vertex:
                  special_vars->has.base_vertex = true;
                  break;

               case nir_intrinsic_load_draw_id:
                  special_vars->has.draw_index = true;
                  break;

               default:
                  break;
               }
               break;

            default:
               break;
            }
         }
      }
   }

   assert(!info->uses_control_barrier && !info->uses_memory_barrier);
}

static void rogue_collect_early_fs_build_data(rogue_build_ctx *ctx,
                                              nir_shader *nir)
{
   const struct shader_info *info = &nir->info;
   struct rogue_fs_build_data *fs_data = &ctx->stage_data.fs;

   nir_foreach_function (func, nir) {
      nir_foreach_block (block, func->impl) {
         nir_foreach_instr (instr, block) {
            switch (instr->type) {
            case nir_instr_type_intrinsic:
               switch (nir_instr_as_intrinsic(instr)->intrinsic) {
               case nir_intrinsic_global_atomic:
               case nir_intrinsic_shared_atomic_img:
               case nir_intrinsic_global_atomic_swap:
               case nir_intrinsic_shared_atomic_swap_img:
                  fs_data->has.atomic_ops = true;
                  break;

               default:
                  break;
               }
               break;

            default:
               break;
            }
         }
      }
   }

   fs_data->early_fragment_tests = info->fs.early_fragment_tests;
   fs_data->depth_layout = info->fs.depth_layout;

   /* TODO: probably should be done somewhere else? */
   if (fs_data->rasterization_samples > VK_SAMPLE_COUNT_1_BIT)
      nir->info.fs.uses_sample_shading = true;

   assert(!info->uses_control_barrier && !info->uses_memory_barrier);
}

static void rogue_collect_early_cs_build_data(rogue_build_ctx *ctx,
                                              nir_shader *nir)
{
   const struct shader_info *info = &nir->info;
   struct rogue_cs_build_data *cs_data = &ctx->stage_data.cs;

   nir_foreach_function (func, nir) {
      nir_foreach_block (block, func->impl) {
         nir_foreach_instr (instr, block) {
            switch (instr->type) {
            case nir_instr_type_intrinsic:
               switch (nir_instr_as_intrinsic(instr)->intrinsic) {
               case nir_intrinsic_load_local_invocation_index:
                  cs_data->has.location_id_x = true;
                  break;

               case nir_intrinsic_load_workgroup_id_x_img:
                  cs_data->has.work_group_id_x = true;
                  break;

               case nir_intrinsic_load_workgroup_id_y_img:
                  cs_data->has.work_group_id_y = true;
                  break;

               case nir_intrinsic_load_workgroup_id_z_img:
                  cs_data->has.work_group_id_z = true;
                  break;

               case nir_intrinsic_load_num_workgroups_base_addr_img:
                  cs_data->has.num_work_groups = true;
                  break;

               case nir_intrinsic_global_atomic:
               case nir_intrinsic_shared_atomic_img:
               case nir_intrinsic_global_atomic_swap:
               case nir_intrinsic_shared_atomic_swap_img:
                  cs_data->has.atomic_ops = true;
                  break;

               default:
                  break;
               }
               break;

            default:
               break;
            }
         }
      }
   }

   assert(!(info->shared_size % 4));
   cs_data->has.shmem_bytes = info->shared_size / 4;

   cs_data->work_size = info->workgroup_size[0] * info->workgroup_size[1] *
                        info->workgroup_size[2];

#if 0
   cs_data->has.barrier = info->uses_control_barrier &&
                          (cs_data->work_size > ROGUE_MAX_INSTANCES_PER_TASK);
#endif

   cs_data->has.barrier = info->uses_control_barrier;
}

static void rogue_collect_early_build_data(rogue_build_ctx *ctx,
                                           nir_shader *nir)
{
   ctx->common_data[nir->info.stage].scratch_size = nir->scratch_size;

   switch (nir->info.stage) {
   case MESA_SHADER_VERTEX:
      return rogue_collect_early_vs_build_data(ctx, nir);

   case MESA_SHADER_FRAGMENT:
      return rogue_collect_early_fs_build_data(ctx, nir);

   case MESA_SHADER_COMPUTE:
      return rogue_collect_early_cs_build_data(ctx, nir);

   default:
      unreachable("Unsupported shader stage.");
   }
}

PUBLIC
const nir_shader_compiler_options *rogue_nir_options(void)
{
   return &nir_options;
}

/**
 * \brief Converts a SPIR-V shader to NIR.
 *
 * \param[in] ctx Shared multi-stage build context.
 * \param[in] entry Shader entry-point function name.
 * \param[in] stage Shader stage.
 * \param[in] spirv_size SPIR-V data length in DWORDs.
 * \param[in] spirv_data SPIR-V data.
 * \param[in] num_spec Number of SPIR-V specializations.
 * \param[in] spec SPIR-V specializations.
 * \return A nir_shader* if successful, or NULL if unsuccessful.
 */
PUBLIC
nir_shader *rogue_spirv_to_nir(rogue_build_ctx *ctx,
                               gl_shader_stage stage,
                               const char *entry,
                               unsigned spirv_size,
                               const uint32_t *spirv_data,
                               unsigned num_spec,
                               struct nir_spirv_specialization *spec)
{
   nir_shader *nir;

   nir = spirv_to_nir(spirv_data,
                      spirv_size,
                      spec,
                      num_spec,
                      stage,
                      entry,
                      &spirv_options,
                      &nir_options);
   if (!nir)
      return NULL;

   ralloc_steal(ctx, nir);

   return nir;
}

/**
 * \brief Compiles a NIR shader into Rogue.
 *
 * Applies rogue nir passes before translating into Rogue.
 *
 * \param[in] ctx Shared multi-stage build context.
 * \param[in] nir NIR shader
 * \return A rogue_shader* if successful, or NULL if unsuccessful.
 */
PUBLIC
rogue_shader *rogue_nir_compile(rogue_build_ctx *ctx, nir_shader *nir)
{
   /* Apply passes. */
   rogue_nir_passes(ctx, nir, nir->info.stage);

   /* Collect initial build data. */
   rogue_collect_early_build_data(ctx, nir);

   return rogue_nir_to_rogue(ctx, nir);
}

#define OPT(pass, ...)                               \
   ({                                                \
      bool _progress = false;                        \
      NIR_PASS(_progress, nir, pass, ##__VA_ARGS__); \
      progress |= _progress;                         \
      _progress;                                     \
   })

static void _rogue_nir_opt_loop(struct rogue_build_ctx *ctx, nir_shader *nir)
{
   bool progress;
   unsigned lower_flrp = (nir->options->lower_flrp16 ? 16 : 0) |
                         (nir->options->lower_flrp32 ? 32 : 0) |
                         (nir->options->lower_flrp64 ? 64 : 0);

   do {
      progress = false;

      OPT(nir_split_array_vars, nir_var_function_temp);
      OPT(nir_shrink_vec_array_vars, nir_var_function_temp);
      OPT(nir_opt_deref);
      if (OPT(nir_opt_memcpy))
         OPT(nir_split_var_copies);
      OPT(nir_lower_vars_to_ssa);
      if (!nir->info.var_copies_lowered)
         OPT(nir_opt_find_array_copies);

      OPT(nir_opt_copy_prop_vars);
      OPT(nir_opt_dead_write_vars);
      OPT(nir_opt_combine_stores, nir_var_all);

#if VEC_ALU
      /* TODO: need to check where this should be called. */
      OPT(nir_lower_alu_width, rogue_vectorize_filter, NULL);
#else
      OPT(nir_lower_alu_to_scalar, NULL, NULL);
      /* OPT(nir_lower_load_const_to_scalar); */
#endif

      OPT(nir_copy_prop);

      OPT(nir_lower_phis_to_scalar, true);

      OPT(nir_copy_prop);
      OPT(nir_opt_dce);
      OPT(nir_opt_cse);
      OPT(nir_opt_combine_stores, nir_var_all);

      /* TODO tweak values, may need to be called once with value set to zero;
       * see intel/brw */
      /* if (!ROGUE_DEBUG(SKIP_CF_OPTS)) */
         OPT(nir_opt_peephole_select, 64, false, true);

      OPT(nir_opt_intrinsics);
      OPT(nir_opt_idiv_const, 8);
      /* OPT(rogue_nir_lower_atan2_img); /1* TODO: check where this should go *1/ */
      OPT(rogue_nir_lower_fquantize2f16);
      /* OPT(rogue_nir_algebraic); /1* TODO: should this go before nir_opt_algebraic? Also add _opt to the name. *1/ */
      /* OPT(nir_opt_algebraic); */

      /* TODO: check if needed */
      OPT(nir_opt_reassociate_bfi);

      OPT(nir_lower_constant_convert_alu_types);
      OPT(nir_opt_constant_folding);

      /* Only do this once. */
      if (lower_flrp) {
         if (OPT(nir_lower_flrp, lower_flrp, false)) {
            OPT(nir_opt_constant_folding);
         }
         lower_flrp = 0;
      }

      OPT(nir_opt_dead_cf);
      if (OPT(nir_opt_trivial_continues)) {
         OPT(nir_copy_prop);
         OPT(nir_opt_dce);
      }

      if (!ROGUE_DEBUG(SKIP_CF_OPTS))
         OPT(nir_opt_if, nir_opt_if_aggressive_last_continue | nir_opt_if_optimize_phi_true_false);

      if (nir->info.stage == MESA_SHADER_FRAGMENT &&
          (nir->info.fs.uses_discard || nir->info.fs.uses_demote)) {
         OPT(nir_opt_conditional_discard);
         /* OPT(nir_opt_move_discards_to_top); */
      }

      if (!ROGUE_DEBUG(SKIP_CF_OPTS) && nir->options->max_unroll_iterations)
         OPT(nir_opt_loop_unroll);

      OPT(nir_opt_remove_phis);
      /* OPT(nir_opt_gcm, false); */
      OPT(nir_opt_undef);
      OPT(nir_lower_undef_to_zero);
      OPT(nir_lower_pack);
   } while (progress);
}

static bool rogue_nir_preprocess_stage(rogue_build_ctx *ctx, nir_shader *nir)
{
   nir_validate_ssa_dominance(nir, "before rogue_nir_preprocess_stage");
   nir_validate_shader(nir, "before rogue_nir_preprocess_stage");

   if (ROGUE_DEBUG(NIR_PASSES)) {
      fputs("before passes\n", stdout);
      nir_print_shader(nir, stdout);
   }

   gl_shader_stage stage = nir->info.stage;
   bool progress = false;

   OPT(nir_scale_fdiv);

   if (stage == MESA_SHADER_COMPUTE)
      NIR_PASS_V(nir, rogue_nir_compute_instance_check);

   /* Inlining. */
   OPT(nir_lower_variable_initializers, nir_var_function_temp);
   OPT(nir_lower_returns);
   if (OPT(nir_inline_functions)) {
      OPT(nir_opt_copy_prop_vars);
      OPT(nir_copy_prop);
   }
   OPT(nir_opt_deref);
   nir_remove_non_entrypoints(nir);

   /* Make sure we lower constant initializers on output variables so that
    * nir_remove_dead_variables below sees the corresponding stores
    */
   OPT(nir_lower_variable_initializers, nir_var_shader_out);

   /* Now that we've deleted all but the main function, we can go ahead and
    * lower the rest of the constant initializers.
    */
   OPT(nir_lower_variable_initializers, nir_var_all);

   /* TODO: probably want to move this group of passes */
   OPT(nir_lower_frexp);
#if VEC_ALU
   OPT(nir_lower_alu_width, rogue_vectorize_filter, NULL);
#else
   OPT(nir_lower_alu_to_scalar, NULL, NULL);
#endif

   /* TODO: any BRN workarounds, etc. */

   /* TODO: lower tex */

   const struct nir_lower_sysvals_to_varyings_options sysvals_to_varyings = {
      .point_coord = true,
      .frag_coord = true,
   };
   /* TODO: edit this to add support for others? */
   OPT(nir_lower_sysvals_to_varyings, &sysvals_to_varyings);

   OPT(nir_lower_global_vars_to_local);

   OPT(nir_split_var_copies);
   OPT(nir_split_per_member_structs);
   OPT(nir_split_struct_vars, nir_var_function_temp);

   _rogue_nir_opt_loop(ctx, nir);

   /* Probably won't be better than doing it in RIR... */
   /* OPT(nir_lower_bit_size, lower_bit_size_callback, NULL); */

   OPT(nir_lower_var_copies);

   /* OPT(nir_opt_large_constants, NULL, 32); */
   OPT(nir_lower_load_const_to_scalar);

   OPT(nir_lower_system_values);
   nir_lower_compute_system_values_options lcsv_opts = {
      .lower_cs_local_id_to_index = true,
   };
   OPT(nir_lower_compute_system_values, &lcsv_opts);

   OPT(rogue_nir_lower_compute_intrinsics);

   /* TODO */
   /* OPT(nir_lower_subgroups, &subgroups_options); */

   /* Lower indirect I/O. */
   OPT(nir_lower_indirect_derefs, nir_var_shader_in | nir_var_shader_out, UINT32_MAX);

   /* TODO: if NOT rogue debug scratch */
#if 0
   {
      /* Lower indirect temporaries (so we don't use scratch).
       * TODO: investigate using scratch, indexed temps?
       */
      OPT(nir_lower_indirect_derefs, nir_var_function_temp, UINT32_MAX);
   }
#endif

   _rogue_nir_opt_loop(ctx, nir);

   nir_validate_shader(nir, "after rogue_nir_preprocess_stage");

   nir_shader_gather_info(nir, nir_shader_get_entrypoint(nir));

   if (ROGUE_DEBUG(NIR_PASSES)) {
      fputs("after rogue_nir_preprocess_stage\n", stdout);
      nir_print_shader(nir, stdout);
   }

   return true;
}

PUBLIC
bool rogue_nir_preprocess(rogue_build_ctx *ctx, bool compute)
{
   if (compute)
      return rogue_nir_preprocess_stage(ctx, ctx->nir[MESA_SHADER_COMPUTE]);

   bool success = true;
   rogue_foreach_graphics_stage (stage) {
      if (!ctx->nir[stage])
         continue;

      success &= rogue_nir_preprocess_stage(ctx, ctx->nir[stage]);
      if (!success)
         break;
   }

   return success;
}

static bool rogue_nir_lower_indirect_derefs(nir_shader *nir)
{
   bool progress = false;
   if (!nir->info.internal && false) {
      NIR_PASS_V(nir, nir_lower_io_to_temporaries, nir_shader_get_entrypoint(nir), true, true);
   NIR_PASS(progress,
            nir,
            nir_lower_vars_to_scratch,
            nir_var_function_temp,
            /* 256, */
            16,
            glsl_get_natural_size_align_bytes);
   nir_variable_mode indirect_mask = nir_var_shader_out | nir_var_shader_in |
                                     nir_var_function_temp;
   NIR_PASS(progress, nir, nir_lower_indirect_derefs, indirect_mask, UINT32_MAX);
   } else {
      NIR_PASS_V(nir, nir_lower_io_to_temporaries, nir_shader_get_entrypoint(nir), true, true);
      OPT(nir_lower_indirect_derefs, nir_var_function_temp, UINT32_MAX);
   }
      OPT(nir_split_var_copies);
      OPT(nir_lower_var_copies);
   return progress;
}

static void rogue_nir_lower_io_to_scalar_early(nir_shader *nir,
                                               nir_variable_mode mask)
{
   bool progress = false;
   nir_lower_array_deref_of_vec_options ladofv_opts =
      nir_lower_direct_array_deref_of_vec_load |
      nir_lower_indirect_array_deref_of_vec_load |
      nir_lower_direct_array_deref_of_vec_store |
      nir_lower_indirect_array_deref_of_vec_store;
   OPT(nir_lower_array_deref_of_vec, mask, ladofv_opts);
   OPT(nir_lower_io_to_scalar_early, mask);
   if (progress) {
      /* Optimize the new vector code and then remove dead vars */
      OPT(nir_copy_prop);
      OPT(nir_opt_shrink_vectors);

      if (mask & nir_var_shader_out) {
         /* Optimize swizzled movs of load_const for nir_link_opt_varyings's
          * constant propagation. */
         OPT(nir_opt_constant_folding);

         /* For nir_link_opt_varyings's duplicate input opt */
         OPT(nir_opt_cse);
      }

      OPT(nir_opt_copy_prop_vars);

      OPT(nir_opt_dce);
      OPT(nir_remove_dead_variables,
          nir_var_function_temp | nir_var_shader_in | nir_var_shader_out,
          NULL);
   }
}

static bool rogue_nir_link_stages(rogue_build_ctx *ctx,
                                  nir_shader *producer,
                                  nir_shader *consumer)
{
   nir_validate_shader(producer, "before rogue_nir_link_stages");
   nir_validate_shader(consumer, "before rogue_nir_link_stages");

   NIR_PASS_V(producer, nir_lower_io_arrays_to_elements_no_indirects, true);
   NIR_PASS_V(consumer, nir_lower_io_arrays_to_elements_no_indirects, false);

   /* Tweaked passes to split structs. */
   NIR_PASS_V(producer, nir_split_struct_vars, nir_var_shader_out);
   NIR_PASS_V(consumer, nir_split_struct_vars, nir_var_shader_in);

   /* NIR_PASS_V(producer, nir_lower_io_arrays_to_elements_no_indirects, true); */
   /* NIR_PASS_V(consumer, nir_lower_io_arrays_to_elements_no_indirects, false); */

   nir_lower_io_arrays_to_elements(producer, consumer);
   nir_validate_shader(producer, "after nir_lower_io_arrays_to_elements");
   nir_validate_shader(consumer, "after nir_lower_io_arrays_to_elements");

   rogue_nir_lower_io_to_scalar_early(producer, nir_var_shader_out);
   rogue_nir_lower_io_to_scalar_early(consumer, nir_var_shader_in);

   _rogue_nir_opt_loop(ctx, producer);
   _rogue_nir_opt_loop(ctx, consumer);


   /* TODO NEXT: re-enable this, see if it creates a new variable for matrix vertex inputs. */ /* ******************** !!!!!! *********************** */
#if 0
   if (producer->info.stage == MESA_SHADER_VERTEX) {
      NIR_PASS_V(producer, nir_lower_io_arrays_to_elements_no_indirects, false);
      NIR_PASS(_, producer, nir_remove_dead_variables, nir_var_shader_in, NULL);

      /* NIR_PASS(_, producer, nir_lower_io_to_vector, nir_var_shader_in); */
      NIR_PASS(_, producer, nir_opt_combine_stores, nir_var_shader_in);
   }
#endif

   if (nir_link_opt_varyings(producer, consumer)) {
      nir_validate_shader(producer, "after nir_link_opt_varyings");
      nir_validate_shader(consumer, "after nir_link_opt_varyings");

      _rogue_nir_opt_loop(ctx, consumer);
   }

   NIR_PASS(_, producer, nir_remove_dead_variables, nir_var_shader_out, NULL);
   NIR_PASS(_, consumer, nir_remove_dead_variables, nir_var_shader_in, NULL);

   bool progress = nir_remove_unused_varyings(producer, consumer);
   nir_compact_varyings(producer, consumer, true);
   if (progress) {
      progress = false;
      NIR_PASS(progress, producer, nir_lower_global_vars_to_local);
#if 1
      if (true || progress) {
         rogue_nir_lower_indirect_derefs(producer);
         /* Remove dead writes, which can remove input loads */
         NIR_PASS(_,
                  producer,
                  nir_remove_dead_variables,
                  nir_var_shader_temp,
                  NULL);
         NIR_PASS(_, producer, nir_opt_dce);
      }
#endif

      progress = false;
      NIR_PASS(progress, consumer, nir_lower_global_vars_to_local);
#if 1
      if (true || progress)
         rogue_nir_lower_indirect_derefs(consumer);
#endif

      _rogue_nir_opt_loop(ctx, producer);
      _rogue_nir_opt_loop(ctx, consumer);
   }

#if 0
   if (consumer->info.stage == MESA_SHADER_FRAGMENT)
      nir_foreach_shader_in_variable (var, consumer)
         if (var->data.interpolation == INTERP_MODE_NONE)
            var->data.interpolation = INTERP_MODE_SMOOTH;
#endif

   NIR_PASS(_, producer, nir_remove_dead_variables, nir_var_shader_temp, NULL);
   NIR_PASS(_, consumer, nir_remove_dead_variables, nir_var_shader_temp, NULL);

   nir_validate_shader(producer, "after nir_compact_varyings");
   nir_validate_shader(consumer, "after nir_compact_varyings");

   NIR_PASS(_, producer, nir_lower_io_to_vector, nir_var_shader_out);
   NIR_PASS(_, producer, nir_opt_combine_stores, nir_var_shader_out);
   NIR_PASS(_, consumer, nir_lower_io_to_vector, nir_var_shader_in);

   /* No arrayed fragment outputs. */
   if (consumer->info.stage == MESA_SHADER_FRAGMENT) {
      NIR_PASS_V(consumer, nir_lower_io_arrays_to_elements_no_indirects, true);
      NIR_PASS(_, consumer, nir_remove_dead_variables, nir_var_shader_out, NULL);

      NIR_PASS(_, consumer, nir_lower_io_to_vector, nir_var_shader_out);
      NIR_PASS(_, consumer, nir_opt_combine_stores, nir_var_shader_out);
   }

   bool has_indirect_inputs =
      (producer->options->support_indirect_inputs >> producer->info.stage) &
      0x1;
   bool has_indirect_outputs =
      (producer->options->support_indirect_outputs >> producer->info.stage) &
         0x1 &&
      producer->xfb_info == NULL;

   NIR_PASS_V(producer,
              nir_lower_io_to_temporaries,
              nir_shader_get_entrypoint(producer),
              !has_indirect_outputs,
              !has_indirect_inputs);

   /* TODO: does the order matter? */
#if 1
   NIR_PASS(_, producer, nir_lower_global_vars_to_local);
   NIR_PASS(_, producer, nir_split_var_copies);
   NIR_PASS(_, producer, nir_lower_var_copies);
#else
   NIR_PASS(_, producer, nir_split_var_copies);
   NIR_PASS(_, producer, nir_lower_var_copies);
   NIR_PASS(_, producer, nir_lower_global_vars_to_local);
#endif

   /* TODO: what to do with this? */
   /* nir_linked_io_var_info io_var_info = nir_assign_linked_io_var_locations(producer, consumer); */

   nir_validate_shader(producer, "after rogue_nir_link_stages");
   nir_validate_shader(consumer, "after rogue_nir_link_stages");

   nir_shader_gather_info(producer, nir_shader_get_entrypoint(producer));
   nir_shader_gather_info(consumer, nir_shader_get_entrypoint(consumer));

   if (ROGUE_DEBUG(NIR_PASSES)) {
      fputs("after rogue_nir_link_stages\n", stdout);
      nir_print_shader(producer, stdout);
      nir_print_shader(consumer, stdout);
   }

   return true;
}

PUBLIC
bool rogue_nir_link(rogue_build_ctx *ctx, bool compute)
{
   if (compute)
      return true;

   bool success = true;
   nir_shader *consumer = NULL;
   rogue_foreach_graphics_stage (stage) {
      if (!ctx->nir[stage])
         continue;

      nir_shader *producer = ctx->nir[stage];
      if (!consumer) {
         consumer = producer;
         continue;
      }

      success &= rogue_nir_link_stages(ctx, producer, consumer);
      if (!success)
         break;

      consumer = producer;
   }

   return success;
}

static bool rogue_nir_lower_stage(nir_shader *nir)
{
   return true;
}

PUBLIC
bool rogue_nir_lower(rogue_build_ctx *ctx, bool compute)
{
   if (compute)
      return rogue_nir_lower_stage(ctx->nir[MESA_SHADER_COMPUTE]);

   bool success = true;
   rogue_foreach_graphics_stage (stage) {
      if (!ctx->nir[stage])
         continue;

      success &= rogue_nir_lower_stage(ctx->nir[stage]);
      if (!success)
         break;
   }

   return success;
}

static bool rogue_nir_postprocess_stage(rogue_build_ctx *ctx, nir_shader *nir)
{
   /* Apply passes. */
   rogue_nir_passes(ctx, nir, nir->info.stage);

   /* Collect initial build data. */
   rogue_collect_early_build_data(ctx, nir);

   return true;
}

PUBLIC
bool rogue_nir_postprocess(rogue_build_ctx *ctx, bool compute)
{
   if (compute)
      return rogue_nir_postprocess_stage(ctx, ctx->nir[MESA_SHADER_COMPUTE]);

   bool success = true;
   rogue_foreach_graphics_stage (stage) {
      if (!ctx->nir[stage])
         continue;

      success &= rogue_nir_postprocess_stage(ctx, ctx->nir[stage]);
      if (!success)
         break;
   }

   return success;
}

static bool rogue_nir_build_stage(rogue_build_ctx *ctx,
                                  nir_shader *nir,
                                  rogue_shader **rogue,
                                  struct util_dynarray *binary)
{
   assert(nir);

   *rogue = rogue_nir_to_rogue(ctx, nir);
   if (!*rogue)
      return false;

   rogue_encode_shader(ctx, *rogue, binary);
   if (!binary->size)
      return false;

   return true;
}

PUBLIC
bool rogue_nir_build(rogue_build_ctx *ctx, bool compute)
{
   if (compute)
      return rogue_nir_build_stage(ctx,
                                   ctx->nir[MESA_SHADER_COMPUTE],
                                   &ctx->rogue[MESA_SHADER_COMPUTE],
                                   &ctx->binary[MESA_SHADER_COMPUTE]);

   bool success = true;
   rogue_foreach_graphics_stage (stage) {
      if (!ctx->nir[stage])
         continue;

      success &= rogue_nir_build_stage(ctx,
                                       ctx->nir[stage],
                                       &ctx->rogue[stage],
                                       &ctx->binary[stage]);

      if (!success)
         break;
   }

   return success;
}
