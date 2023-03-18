# Porous Media: Percolation & Random Cluster Models


## Project

> Student: Adrian Ivan\
> Candidate BSc CS 2023\
> West University, Timisoara
>
> Supervisors:\
> Prof. Dr. Daniela Zaharie\
> Dr. med. Leonard Mada (Syonic SRL)
>
> URL: https://github.com/adrian0010/Percolation
> 
> Based on Previous Projects (2020-2022)


## Processes

### Coupled Pore Process
- TODO: Pore & Block Process;

### Coupled Process: 2 Matrices
- function as.grid.m2(m1, m2, t, p=0.5, val = c(-1, 0));
- where: t = Mixing parameter;


## Statistics

### Number of Channels (Clusters)
- percolating clusters;
- non-percolating, but connected to the boundary;
- non-connected to the boundary;

### Length
- Average of Minimal length of percolating clusters;
- Note: a percolating cluster can have multiple outflows;

### Area
- percolating clusters;
- non-percolating, but connected to the boundary;
- non-connected to the boundary (i.e. fully bounded);
- area of background material;

### Minimal Diameter
- Minimal Diameter: V, H, absolute minimum;

### Dead-End Components
- area of dead-end branches (sub-channels) inside a percolating cluster;
- dead-end area with specified maximum inflow (e.g. inflow of 1 unit: should be easier to define and compute);


### Contact Surface
- Length of the Contact Surface: inner boundaries inside the cluster are counted as well;
- Length of Contact Hull Surface;


### Special Materials: Linear Channels with Blocks & Pores
> rgrid.channel()
- density of percolating channels;
- minimal & maximal length of percolating channels;
- minimal & maximal height of percolating channels: the cluster may span multiple linear channels;


===============

## Bibliography


### Introduction

1. Percolation: a Mathematical Phase Transition.
  > https://www.youtube.com/watch?v=a-767WnbaCQ

2. Hugo Duminil-Copin. Sixty years of percolation.
  > https://www.ihes.fr/~duminil/publi/2018ICM.pdf


### Applications

1. Various applications of porous materials in Chemistry & Physics, including Catalysis, Chromatography, Molecular Sieves.

2. Fluid Mechanics 101: CFD Analysis of a Lead-Cooled Nuclear Reactor.
  > https://www.youtube.com/watch?v=u17fjAjAGvQ
- modeling of the core as a porous material:
  - see 33:30 - 39:25 (Pattern) - 40:40 (Heat losses; actual use of porous media) - 43:40;
  - pump as porous zone: 43:40;
  - heat exchanger as block of porous material: 45:20;

3. NPTEL: Lec 10 Percolation Theory and applications in biological tissues
  > https://www.youtube.com/watch?v=0WeiTnhUoGQ
- CNT = Carbon Nano-Tubes
- CNF = Carbon Nano-Fibers


### Advanced

1. I. Manolescu, L.V. Santoro: Widths of crossings in Poisson Boolean percolation. (2022)
  > https://arxiv.org/abs/2211.11661


**Lectures:**

1. IHES: Eveliina Peltola - On crossing probabilities in critical random-cluster models. (2022)
  > https://www.youtube.com/watch?v=Zg8g_0KF8SA

2. IAS: On Crossing Probabilities in Critical Random-Cluster Models - Eveliina Peltola. (2023)
  > https://www.youtube.com/watch?v=1Z9acHxYFoA

3. IMS Medallion Lecture: "Deformed Polynuclear Growth in (1+1) Dimensions" - Alexei Borodin. (2022)
  > https://www.youtube.com/watch?v=KZntINdhTM4

4. CRM: Jeremy Quastel: Polynuclear growth and the Toda lattice. (2022)
  > https://www.youtube.com/watch?v=yWcAEp_CvCM&list=PLHaWeIntAtAJ-isWJZxdMGAAicT3lBEtT&index=3
