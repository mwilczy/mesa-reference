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
 * \file rogue_nir_opt_preamble.c
 *
 * \brief Contains the rogue_nir_opt_preamble pass.
 */

/* TODO: actual costs. */

static void def_size(nir_def *def, unsigned *size, unsigned *align)
{
   unsigned bit_size = def->bit_size < 32 ? 32 : def->bit_size;
   *size = DIV_ROUND_UP(bit_size, 32) * def->num_components;
   *align = 1;
}

static float instr_cost(UNUSED nir_instr *instr, UNUSED const void *data)
{
   return 1.0f;
}

static float rewrite_cost(UNUSED nir_def *def, UNUSED const void *data)
{
   return 0.0f;
}

static bool avoid_instr(UNUSED const nir_instr *instr, UNUSED const void *data)
{
   return false;
}

PUBLIC
bool rogue_nir_opt_preamble(nir_shader *shader, rogue_build_ctx *ctx)
{
   gl_shader_stage stage = shader->info.stage;

   if (shader->info.internal ||
       (stage != MESA_SHADER_FRAGMENT && stage != MESA_SHADER_VERTEX)) {
      return false;
   }

   nir_opt_preamble_options preamble_options = {
      .drawid_uniform = true,
      .subgroup_size_uniform = true,
      .def_size = def_size,
      .preamble_storage_size = 128,
      .instr_cost_cb = instr_cost,
      .rewrite_cost_cb = rewrite_cost,
      .avoid_instr_cb = avoid_instr,
      .cb_data = ctx,
   };

   unsigned *preamble_size = &ctx->common_data[stage].preamble.shareds;
   return nir_opt_preamble(shader, &preamble_options, preamble_size);
}
