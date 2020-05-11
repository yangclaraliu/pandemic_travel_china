#!/usr/bin/python3
import sys
from glob import glob
from pandas.io.parsers import read_csv
import igraph as ig
from leidenalg import find_partition_temporal, ModularityVertexPartition
from re import compile
import numpy as np

if (len(sys.argv) == 1):
    print("usage: ./leiden.py path/to/outputdir")
    sys.exit()

pth = sys.argv[1]
tar = pth + "/digest.csv"
isw = float(sys.argv[2])

fls = sorted(glob(pth + "/*_mat.csv"))

rgx = compile(r"^.*/([^/]+)_mat.csv$")

def mkgraph(fl):
    al = read_csv(fl)
    mt = al.values[:,2:]
    mt[np.isnan(mt)] = 0
    G = ig.Graph.Adjacency((mt > 0).tolist())
    G.es['weight'] = mt[mt.nonzero()]
    G.vs['id'] = al.columns[2:].tolist()
    return G

def extractids(fl):
    al = read_csv(fl)
    return al.columns[2:].tolist()


allgraphs = [mkgraph(f) for f in fls]

tpart, improvement = find_partition_temporal(allgraphs, ModularityVertexPartition, interslice_weight = isw, n_iterations = -1)

dates = [rgx.sub(r"\1", f) for f in fls]

ids = extractids(fls[0])

from pandas import DataFrame

for ind in range(len(tpart)):
    df = DataFrame({ 'date' : dates[ind], 'vertex' : ids, 'cluster' : tpart[ind] })
    df.to_csv(tar, mode="a", index = False, header = False)

# for fl in sorted(fls):
#     al = read_csv(fl)
#     mt = al.values[:,2:]
#     mt[np.isnan(mt)] = 0
#     G = ig.Graph.Adjacency((mt > 0).tolist())
#     G.es['weight'] = mt[mt.nonzero()]
#     part = find_partition(G, ModularityVertexPartition, weights = G.es['weight'])
#     df = DataFrame({ 'date' : rgx.sub(r"\1", fl), 'vertex' : al.columns[2:].tolist(), 'cluster' : part.membership })
#     df.to_csv(tar, mode="a", index = False, header = False)
