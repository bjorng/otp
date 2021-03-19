# coding=utf-8
#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2021. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#

import re
import lldb

unquoted_atom_re = re.compile(u'^[a-zß-öø-ÿ][a-zA-Zß-öø-ÿ0-9@]*$')

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('type format add -f hex Eterm')
    debugger.HandleCommand('type format add -f hex BeamInstr')
    debugger.HandleCommand('type summary add -F etp.eterm_flat_summary Eterm')

def eterm_flat_summary(valobj, internal_dict):
    val = valobj.unsigned
    tag = val & 0x3
    if tag == 0x1:
        return '#Cons<%#x>' % val
    elif tag == 0x2:
        return '#Boxed<%#x>' % val
    elif tag == 0x3:
        return imm_summary(valobj)
    elif val == 0x0:
        return '<the non-value>'
    elif val == 0x4:
        return '<the non-value debug>'
    else:
        return cp_summary(valobj)

def imm_summary(valobj):
    val = valobj.unsigned
    if (val & 0x3) != 3:
        return '#NotImmediate<%#x>' % val
    tag = val & 0xF
    if tag == 0x3:
        return pid_summary(valobj)
    elif tag == 0x7:
        return port_summary(valobj)
    elif tag == 0xF:
        return str(val >> 4)
    elif tag == 0xB:
        # Immediate2
        tag2 = val & 0x3F
        if tag2 == 0x0B:
            return atom_summary(valobj)
        elif tag2 == 0x1B:
            return '#Catch<%#x>' % val >> 6
        elif val == NIL(valobj.target):
            return '[]'
    return '#UnknownImmediate<%#x>' % val

# Continuation pointers

def cp_summary(valobj):
    mfa = find_range(valobj)
    print(mfa)
    return '#Pc<%#x>' % valobj.unsigned

# Pids and ports

def pid_summary(valobj):
    val = valobj.unsigned
    if (val & 0xF) == 0x3:
        target = valobj.target
        if etp_arch_bits(target) == 64 and not etp_halfword(target):
            if etp_big_endian(target):
                data = (val >> 36) & 0x0FFFFFFF
            else:
                data = (val >> 4) & 0x0FFFFFFF
        else:
            data = pixdata2data(valobj)
        return '<0.%u.%u>' % (data & 0x7FFF, (data >> 15) & 0x1FFF)
    else:
        return '#NotPid<%#x>' % val

def port_summary(valobj):
    val = valobj.unsigned
    if (val & 0xF) == 0x7:
        target = valobj.target
        if etp_arch_bits(target) == 64 and not etp_halfword(target):
            if etp_big_endian(target):
                data = (val >> 36) & 0x0FFFFFFF
            else:
                data = (val >> 4) & 0x0FFFFFFF
        else:
            data = pixdata2data(valobj)
        return '#Port<0.%u>' % data
    else:
        return '#NotPort<%#x>' % val

# Strings and atoms

def atom_summary(valobj):
    val = valobj.unsigned
    if (val & 0x3F) == 0x0B:
        name = atom_name(atom_tab(valobj))
        if unquoted_atom_re.match(name):
            return str(name)
        else:
            return quoted_name(name, "'")
    else:
        return '#NotAtom<%#x>' % val

def atom_name(entry):
    name = entry.GetChildMemberWithName('name')
    length = entry.GetChildMemberWithName('len').unsigned
    data = name.GetPointeeData(0, length).uint8s
    return unicode(''.join(map(chr, data)), 'utf-8')

def quoted_name(name, quote):
    return quote + ''.join(map(lambda c: quoted_char(c, quote), name)) + quote

def quoted_char(c, quote):
    point = ord(c)
    if c == quote:
        return '\\' + quote
    elif point == 0x08:
        return '\\b'
    elif point == 0x09:
        return '\\t'
    elif point == 0x0A:
        return '\\n'
    elif point == 0x0B:
        return '\\v'
    elif point == 0x0C:
        return '\\f'
    elif point == 0x0D:
        return '\\e'
    elif point >= 0x20 and point <= 0x7E or point >= 0xA0:
        return c
    elif (point > 0xFF):
        return '#NotChar<%#x>' % c
    else:
        return '\\%03o' % point

# Constants

MI_FUNCTIONS = 12
MI_NUM_FUNCTIONS = 0

def NIL(target):
    if etp_arch_bits(target) == 32:
        return 0xFFFFFFFB
    else:
        return 0xFFFFFFFFFFFFFFFB

# Types

def Atom(target):
    return target.FindFirstType('Atom')

def Eterm(target):
    return target.FindFirstType('Eterm')

def Range(target):
    return target.FindFirstType('Range')

# Globals

def erts_atom_table(target):
    return global_var('erts_atom_table', target)

def erts_proc(target):
    return global_var('erts_proc', target)

def etp_arch_bits(target):
    return global_var('etp_arch_bits', target).unsigned

def etp_big_endian(target):
    return bool(global_var('etp_big_endian', target).unsigned)

def etp_halfword(target):
    return bool(global_var('etp_halfword', target).unsigned)

def the_active_code_index(target):
    return global_var('the_active_code_index', target)

# Functions

def atom_tab(valobj):
    idx = valobj.unsigned
    target = valobj.target
    seg = erts_atom_table(target).GetChildMemberWithName('seg_table')
    slot = offset(idx >> 16, seg)
    entry = offset(idx >> 6 & 0x3FF, slot)
    return entry.Cast(Atom(target).GetPointerType())

def erts_lookup_function_info(valobj):
    r = find_range(valobj)
    if r is None:
        return None
    pc = valobj.unsigned
    target = valobj.target
    start = r.GetChildMemberWithName('start')
    low = offset(MI_FUNCTIONS, start).address_of
    high = offset(offset(MI_NUM_FUNCTIONS, start).unsigned, low).address_of
    while low.unsigned < high.unsigned:
        length = (high.unsigned - low.unsigned)
        mid = offset((high.unsigned - low.unsigned) / 2, low).address_of
        if pc < mid.deref.unsigned:
            high = mid
        elif pc < offset(1, mid).unsigned:
            ptr = mid.deref.Cast(Eterm(target).GetPointerType())
            Mod = offset(2, ptr)
            Name = offset(3, ptr)
            Arity = offset(4, ptr)
            return Mod, Name, Arity
        else:
            low = offset(1, mid).address_of
    return None

def find_range(valobj):
    pc = valobj.unsigned
    target = valobj.target
    active = the_active_code_index(target).unsigned
    ranges = offset(active, global_var('r', target))
    n = ranges.GetChildMemberWithName('n').unsigned
    low = ranges.GetChildMemberWithName('modules')
    high = offset(n, low).address_of
    range_type = Range(target)
    range_pointer_type = range_type.GetPointerType()
    mid = ranges.GetChildMemberWithName('mid').Cast(range_pointer_type)
    while low.unsigned < high.unsigned:
        if pc < mid.GetChildMemberWithName('start').unsigned:
            high = mid
        elif pc > mid.GetChildMemberWithName('end').unsigned:
            low = offset(1, mid).address_of.Cast(range_pointer_type)
        else:
            return mid
        length = (high.unsigned - low.unsigned) / range_type.size
        mid = offset(length / 2, low).address_of
    return None

def pixdata2data(valobj):
    pixdata = valobj.unsigned
    proc = erts_proc(target)
    ro = proc.GetChildMemberWithName('r').GetChildMemberWithName('o')
    pix_mask = ro.GetChildMemberWithName('pix_mask').unsigned
    pix_cl_mask = ro.GetChildMemberWithName('pix_cl_mask').unsigned
    pix_cl_shift = ro.GetChildMemberWithName('pix_cl_shift').unsigned
    pix_cli_mask = ro.GetChildMemberWithName('pix_cli_mask').unsigned
    pix_cli_shift = ro.GetChildMemberWithName('pix_cli_shift').unsigned
    data = pixdata & ~pix_mask
    data |= (pixdata >> pix_cl_shift) & pix_cl_mask
    data |= (pixdata & pix_cli_mask) << pix_cli_shift
    return data

# LLDB utils

def global_var(name, target):
    return target.FindGlobalVariables(name, 1)[0]

def offset(i, valobj):
    return valobj.GetChildAtIndex(i, lldb.eNoDynamicValues, True)
