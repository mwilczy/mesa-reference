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

#ifndef ROGUE_NIR_FMT_UTILS_H
#define ROGUE_NIR_FMT_UTILS_H

#include "nir/nir.h"
/* #include "nir/nir_builder.h" */
/* #include "nir/nir_builtin_builder.h" */
/* #include "nir/nir_search_helpers.h" */
/* #include "nir/nir_format_convert.h" */
/* #include "rogue.h" */
/* #include "util/macros.h" */

/**
 * \file rogue_nir_fmt_utils.h
 *
 * \brief Contains utilities for format transformations.
 */

nir_def *
fmt_pack_scalar(nir_builder *b,
                nir_def *base,
                nir_def *value,
                unsigned dest_bits,
                nir_def *chan,
                const struct util_format_channel_description *chan_desc,
                bool is_texture_pack);

nir_def *
fmt_unpack_scalar(nir_builder *b,
                  nir_def *raw_load,
                  unsigned dest_bits,
                  enum pipe_swizzle chan,
                  const struct util_format_channel_description *chan_desc);

nir_def *
fmt_colorspace_transform_scalar(nir_builder *b,
                                nir_def *value,
                                unsigned bits,
                                enum pipe_swizzle chan,
                                const struct util_format_description *fmt_desc,
                                bool to_colorspace);

#endif /* ROGUE_NIR_FMT_UTILS_H */
