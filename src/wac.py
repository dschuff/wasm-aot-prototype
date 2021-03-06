#!/usr/bin/env python
#
# Copyright 2016 WebAssembly Community Group participants
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


import argparse
import os
import subprocess
import sys

RUNTIME_LIB = 'wart'
OS = os.uname()[0]
if OS.startswith('Linux'):
  LINKER_SYM_FLAG = []#'-Wl,--section-start=.membase=0x100000000'
elif OS.startswith('Darwin'):
  LINKER_SYM_FLAG = []


def find_runtime_dir(start_dir):
  lib_name = 'lib' + RUNTIME_LIB + '.a'
  if os.path.exists(os.path.join(start_dir, lib_name)):
    return os.path.abspath(start_dir)
  for d in [os.path.join(start_dir, x) for x in os.listdir(start_dir)
            if os.path.isdir(os.path.join(start_dir, x))]:
    f = find_runtime_dir(d)
    if f:
      return f
  return None

def log_call_internal(verbose, args):
  if verbose:
    print >> sys.stderr, ' '.join(args)
  try:
    subprocess.check_call(args)
  except subprocess.CalledProcessError:
    print >> sys.stderr, 'Command Failed:'
    print >> sys.stderr, ' '.join(args)
    sys.exit(1)

def Main(argv):
  parser = argparse.ArgumentParser(
      description="End-to-end compiler driver for waot tests")
  parser.add_argument('-o', '--output', help='Output file', default='a.out')
  parser.add_argument('-s', '--spec-test-script',
                      help='Run translator in spec test script mode',
                      action='store_true')
  parser.add_argument('-v', '--verbose', help='Log calls',
                      action='store_true')
  parser.add_argument('inputs', metavar='INPUT', type=str, nargs='+',
                      help='input file')
  options = parser.parse_args(argv)
  def log_call(args):
    return log_call_internal(options.verbose, args)

  file_dir = os.path.dirname(os.path.abspath(__file__))
  runtime_libdir = (
      find_runtime_dir(file_dir) or
      find_runtime_dir(os.path.join(os.path.dirname(file_dir), 'out')))
  if not runtime_libdir:
    print 'Could not locate', 'lib' + RUNTIME_LIB + '.a'
    return 1

  outdir = os.path.dirname(os.path.abspath(options.output))

  objs = []
  for input in options.inputs:
    ll_temp = os.path.join(outdir, os.path.basename(input)) + '.ll'
    o_temp = os.path.join(outdir, os.path.basename(input)) + '.o'
    wat_flags = ['-o', ll_temp, input]
    if options.spec_test_script:
      wat_flags.append('-spec-test-script')
    log_call([os.path.join(runtime_libdir, 'wat')] + wat_flags)
    log_call(['llc', ll_temp, '-O0', '-filetype=obj', '-o', o_temp])
    objs.append(o_temp)

  log_call(['gcc', '-o', options.output] + objs + LINKER_SYM_FLAG +
           ['-rdynamic',
            '-L'+runtime_libdir, '-l'+RUNTIME_LIB, '-lm'])

if __name__ == '__main__':
  sys.exit(Main(sys.argv[1:]))
