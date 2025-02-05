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

   # Bitfield insert/extract special-case handling.
   (('bitfield_insert', 'base', 'insert', 'offset', 0), 'base'),
   (('ubitfield_extract', 'value', 'offset', 0), 0),
   (('ibitfield_extract', 'value', 'offset', 0), 0),

   (('iadd@64', a, '#offset(is_upper_half_zero)'), ('uadd64_32_img', a, ('u2u32', 'offset'))),
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

def pack_2x16(fmt):
   return (
      ('pack_' + fmt + '_2x16', a),
      ('vec2',
         ('unpack_' + fmt + '_2x16_split_x', a),
         ('unpack_' + fmt + '_2x16_split_y', a)
      )
   )

def pack_4x8(fmt):
   return (
      ('pack_' + fmt + '_4x8', a),
      ('pack_' + fmt + '_4x8_field',
         ('pack_' + fmt + '_4x8_field',
            ('pack_' + fmt + '_4x8_field',
               ('pack_' + fmt + '_4x8_field',
                  0,
               'a.x', 0),
            'a.y', 1),
         'a.z', 2),
      'a.w', 3)
   )

def pack_2x16(fmt):
   return (
      ('pack_' + fmt + '_2x16', a),
      ('pack_' + fmt + '_2x16_field',
         ('pack_' + fmt + '_2x16_field',
            0,
         'a.x', 0),
      'a.y', 1)
   )

split_packs = [
   pack_2x16('snorm'),
   pack_4x8('snorm'),
   pack_2x16('unorm'),
   pack_4x8('unorm'),
   pack_2x16('sscaled'),
   pack_4x8('sscaled'),
   pack_2x16('uscaled'),
   pack_4x8('uscaled'),
   pack_2x16('half'),

   (
      ('pack_half_2x16_split', a, b),
      ('pack_half_2x16_field',
         ('pack_half_2x16_field',
            0,
         a, 0),
      b, 1)
   )
]

algebraic_late += split_packs

# Only 4x8 and 2x16 field packs are natively supported in hardware.
# Lower the other to the full op + bitfield extract/insert ops

# ('bitfield_insert', 'base', 'insert', 'offset', 'bits')
# ('ubitfield_extract', 'base', 'offset', 'bits')

# TODO: pck.prog to no longer require this.
def legalise_r10g10b10a2(fmt):
   return [
      (
         ('pack_' + fmt + '_r10g10b10a2_field', 'base', 'insert', 0),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_' + fmt + '_r10g10b10a2', ('vec4', 'insert', 0, 0, 0)),
               0, 10),
            0, 10)
         )
      ),
      (
         ('pack_' + fmt + '_r10g10b10a2_field', 'base', 'insert', 1),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_' + fmt + '_r10g10b10a2', ('vec4', 0, 'insert', 0, 0)),
               10, 10),
            10, 10)
         )
      ),
      (
         ('pack_' + fmt + '_r10g10b10a2_field', 'base', 'insert', 2),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_' + fmt + '_r10g10b10a2', ('vec4', 0, 0, 'insert', 0)),
               20, 10),
            20, 10)
         )
      ),
      (
         ('pack_' + fmt + '_r10g10b10a2_field', 'base', 'insert', 3),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_' + fmt + '_r10g10b10a2', ('vec4', 0, 0, 0, 'insert')),
               30, 2),
            30, 2)
         )
      )
   ]

def legalise_r11g11b10f():
   return [
      (
         ('pack_r11g11b10f_field', 'base', 'insert', 0),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_r11g11b10f', ('vec3', 'insert', 0, 0)),
               0, 11),
            0, 11)
         )
      ),
      (
         ('pack_r11g11b10f_field', 'base', 'insert', 1),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_r11g11b10f', ('vec3', 0, 'insert', 0)),
               11, 11),
            11, 11)
         )
      ),
      (
         ('pack_r11g11b10f_field', 'base', 'insert', 2),
         (
            ('bitfield_insert', 'base',
               ('ubitfield_extract',
                  ('pack_r11g11b10f', ('vec3', 0, 0, 'insert')),
               22, 10),
            22, 10)
         )
      )
   ]

legalise_field_packs = []
legalise_field_packs += legalise_r10g10b10a2('snorm')
legalise_field_packs += legalise_r10g10b10a2('unorm')
legalise_field_packs += legalise_r10g10b10a2('sscaled')
legalise_field_packs += legalise_r10g10b10a2('uscaled')
legalise_field_packs += legalise_r11g11b10f()

algebraic_late += legalise_field_packs

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
