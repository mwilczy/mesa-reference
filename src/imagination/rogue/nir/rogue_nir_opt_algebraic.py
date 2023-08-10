# Copyright © 2023 Imagination Technologies Ltd.

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice (including the next
# paragraph) shall be included in all copies or substantial portions of the
# Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import argparse
import sys
import math

a = 'a'
b = 'b'

algebraic_late = [
   # Change extract ops into bitfield ops.
   (('extract_u16', 'a@32', 'b@32'), ('ubitfield_extract', a, ('imul', b, 16), 16)),
   (('extract_i16', 'a@32', 'b@32'), ('ibitfield_extract', a, ('imul', b, 16), 16)),

   (('extract_u8', 'a@32', 'b@32'), ('ubitfield_extract', a, ('imul', b, 8), 8)),
   (('extract_i8', 'a@32', 'b@32'), ('ibitfield_extract', a, ('imul', b, 8), 8)),

]

# Split unpack ops.
# TODO: Only split if not all components are being used.
def unpack_2x16(fmt):
   return (
      ('unpack_' + fmt + '_2x16', a),
      ('vec2',
         ('unpack_' + fmt + '_2x16_split_x', a),
         ('unpack_' + fmt + '_2x16_split_y', a)
      )
   )

def unpack_4x8(fmt):
   return (
      ('unpack_' + fmt + '_4x8', a),
      ('vec4',
         ('unpack_' + fmt + '_4x8_split_x', a),
         ('unpack_' + fmt + '_4x8_split_y', a),
         ('unpack_' + fmt + '_4x8_split_z', a),
         ('unpack_' + fmt + '_4x8_split_w', a)
      )
   )

def unpack_r10g10b10a2(fmt):
   return (
      ('unpack_' + fmt + '_r10g10b10a2', a),
      ('vec4',
         ('unpack_' + fmt + '_r10g10b10a2_split_x', a),
         ('unpack_' + fmt + '_r10g10b10a2_split_y', a),
         ('unpack_' + fmt + '_r10g10b10a2_split_z', a),
         ('unpack_' + fmt + '_r10g10b10a2_split_w', a)
      )
   )

def unpack_r11g11b10f():
   return (
      ('unpack_r11g11b10f', a),
      ('vec3',
         ('unpack_r11g11b10f_split_x', a),
         ('unpack_r11g11b10f_split_y', a),
         ('unpack_r11g11b10f_split_z', a)
      )
   )

split_unpacks = [
   unpack_2x16('snorm'),
   unpack_4x8('snorm'),
   unpack_r10g10b10a2('snorm'),
   unpack_2x16('unorm'),
   unpack_4x8('unorm'),
   unpack_r10g10b10a2('unorm'),
   unpack_2x16('sscaled'),
   unpack_4x8('sscaled'),
   unpack_r10g10b10a2('sscaled'),
   unpack_2x16('uscaled'),
   unpack_4x8('uscaled'),
   unpack_r10g10b10a2('uscaled'),
   unpack_2x16('half'),
   unpack_r11g11b10f(),
]

algebraic_late += split_unpacks

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--import-path', required=True)
    args = parser.parse_args()
    sys.path.insert(0, args.import_path)
    run()

def run():
    import nir_algebraic  # pylint: disable=import-error
    print('#include "rogue.h"')
    print(nir_algebraic.AlgebraicPass("rogue_nir_opt_algebraic_late", algebraic_late).render())

if __name__ == '__main__':
    main()
