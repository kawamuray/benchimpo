#!/usr/bin/env python
import sys
import argparse
import os
import os.path
import subprocess
import shutil
import tempfile
import time
import signal

FIELDS = [
  ("ExpectCount",    "x1y1"),
  ("Count",    "x1y1"),
  ("Failed",   "x1y1"),
  ("MaxTime",  "x1y2"),
  ("MinTime",  "x1y2"),
  ("MeanTime", "x1y2"),
  ("Req/sec",  "x1y2"),
  ]

ARG_PARSER = argparse.ArgumentParser(prog=sys.argv[0],
                                     usage="%(prog)s [options] DATA_DIR -- BENCHIMPO_ARGS")
ARG_PARSER.add_argument('-i', '--interval', default="0.1",
                        help="Plot interval(secs)")
ARG_PARSER.add_argument('-m', '--mode',
                        help="Graph mode. MODE must be one of: single(all in single graph, default), mls ulti(graph per scenario)")
ARG_PARSER.add_argument('-f', '--force', action='store_true',
                        help="Allow overwrite data directory if necessary")
ARG_PARSER.add_argument('-o', '--output',
                        default=','.join(x[0] for x in FIELDS),
                        help="Output fields separated by comma")
ARG_PARSER.add_argument('data_dir', help="Path to data directory")
ARG_PARSER.add_argument('benchimpo_args', nargs='+')

def Main():
  args = ARG_PARSER.parse_args()
  data_dir = args.data_dir
  show_fields = set(args.output.split(','))

  if os.path.exists(data_dir):
    if args.force:
      shutil.rmtree(data_dir)
    else:
      sys.exit("Directory %s already exists(use -f to overwrite)" % data_dir)
  os.mkdir(data_dir)

  proc_bm = subprocess.Popen(["benchimpo"] + args.benchimpo_args)

  time.sleep(0.1)
  graph_specs = ['"%s" using 1:%d title "%s" with lp axis %s' %
                 (os.path.join(data_dir, s), i, label, axis)
                 for s in os.listdir(data_dir)
                 for i, (label, axis) in enumerate(FIELDS, start=2)
                 if label in show_fields]
  configs = [
    'set ytics nomirror',
    'set y2tics',
    'set xlabel "Clock(secs)"',
    'set ylabel "Requests"',
    'set y2label "Elapsed(secs)"',
    "plot %s" % ",".join(graph_specs),
    ]
  tmpfile = tempfile.NamedTemporaryFile(prefix="benchimpo-", suffix=".gp")
  tmpfile.write("\n".join([
    "pause %s" % args.interval,
    "replot",
    "reread",
    ]))
  tmpfile.flush()

  proc_gp = subprocess.Popen(["gnuplot", "-persist", "-e", ";".join(configs), tmpfile.name])

  signal.pause()

  proc_bm.wait()
  if proc_gp.poll() is None:
    proc_gp.terminate()
  proc_gp.wait()

if __name__ == '__main__':
  Main()
