#!/usr/bin/env python
import argparse
import os
import subprocess
import sys

RUNTIME_LIB = 'wart'

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

  runtime_libdir = (
      find_runtime_dir(
          os.path.dirname(os.path.dirname(os.path.abspath(__file__)))) or
      find_runtime_dir(os.getcwd()))
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
    log_call(['wat'] + wat_flags)
    log_call(['llc', ll_temp, '-filetype=obj', '-o', o_temp])
    objs.append(o_temp)

  log_call(['gcc', '-o', options.output] + objs +
           ['-L'+runtime_libdir, '-l'+RUNTIME_LIB])

if __name__ == '__main__':
  sys.exit(Main(sys.argv[1:]))
