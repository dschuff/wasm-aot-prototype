#!/usr/bin/env python
import argparse
import os
import subprocess
import sys

RUNTIME_LIB = 'waot_runtime'

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

def log_call(verbose, args):
  if verbose:
    print >> sys.stderr, ' '.join(args)
  subprocess.check_call(args)

def Main(argv):
  parser = argparse.ArgumentParser(
      description="End-to-end compiler driver for waot tests")
  parser.add_argument('-o', dest='output', help='Output file', default='a.out')
  parser.add_argument('-v', dest='verbose', help='Log calls',
                      action='store_true')
  parser.add_argument('inputs', metavar='INPUT', type=str, nargs='+',
                      help='input file')
  options = parser.parse_args(argv)

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
    log_call(options.verbose, ['waot', '-o', ll_temp, input])
    log_call(options.verbose, ['llc', ll_temp, '-filetype=obj', '-o', o_temp])
    objs.append(o_temp)

  log_call(options.verbose, ['gcc', '-o', options.output] + objs +
           ['-L'+runtime_libdir, '-l'+RUNTIME_LIB])

if __name__ == '__main__':
  sys.exit(Main(sys.argv[1:]))
