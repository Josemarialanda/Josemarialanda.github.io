---
layout: post
title: "Mathematical analysis of a simple SIR model"
author: "José María Landa Chávez"
categories: Math
tags: [differential equations, mathematical modelling]
image: SIR_MODEL.png
---

## Context

A disease is a particular abnormal condition that negatively affects the structure or function of all or part of an organism, and that is not due to any immediate external injury. Diseases are normally transmitted through direct contact from individual to individual. 

* At the beginning of an epedemic, only a fraction of the population is contagious (**Infected Population**).

* With time, a disease will spread in a given closed population. 

* Everyone in the population (except those initially infected) can be infected (**Susceptible Population**).

* The infected will have to endure the entire process of the disease and have only two possible outcomes: recovery (**Recovered Population**) or death. 

## Model Construction

We will denote the Susceptible Population at a time $t$ as $S(t)$, the Infected Population as $I(t)$ and the Recovered Population as $R(t)$.

The mathematical equation that will determine the rate of change (per unit of time) of infected individuals can be derived from an initial discrete approximation:

Let $I(t_1)$ and $I(t_2)$ denote the infected population with a difference of one unit of time, that is $\Delta t = t_2-t_1=1$. This way,

$$
I(t_2)=I(t_1)+(\beta S(t_1))I(t_1)-\gamma I(t_1)
$$


Writting this difference explicitly:

$$
I(t_2)-I(t_1)=\left[\left(\beta S(t_1)\right)I(t_1)-\gamma I(t_1)\right]
$$

We can write the rate of change

$$
\frac{\Delta I(t)}{\Delta t}=\left[\frac{I(t_2)-I(t_1)}{t_2-t_1}\right]
$$

Finally, if we let $\Delta t \rightarrow 0$, we obtain the differential equation:

$$
\frac{dI}{dt}=\beta S I - \gamma I
$$

Using the same logic, we can derive the equation for the Susceptible Population

$$
\frac{dS}{dt}=-\beta S I,
$$

and for the Recovered Population:

$$
\frac{dR}{dt}=-\gamma I
$$

To make things easier, we will consider a population that is closed and constant, i.e $N(t)=N\in\mathbb{R} \space \space \space \forall \space \space t$.

If we take demography into consideration, that is we assume a mortality and natality rate different from zero in the population but wishing to conserve a constant population (population growth rate equal to zero, natality rate $\mu$ and mortality rate $\mu$), we can formulate the following system of differential equations:

$$
\begin{array}{rcr} 

\frac{dS}{dt} & = & \mu N - \beta S I - \mu S \\ 
\frac{dI}{dt} & = & \beta S I - \gamma I - \mu I\\
\frac{dR}{dt} & = & \gamma I - \mu R

\end{array}
$$

By linearity of the derivative operator:

$$
\left(\frac{dS}{dt}+\frac{dI}{dt}+\frac{dR}{dt}\right)=\frac{d}{dt}\left(S+I+R\right)=\frac{dN}{dt}=0 \space \space \space and \space \space \space N =S+I+R
$$

Allowing us to write the previous system as follows:

$$
\begin{array}{rcr} 

\frac{dS}{dt} & = & \mu\left(I+R\right) - \beta S I \\ 
\frac{dI}{dt} & = & \beta S I - \gamma I - \mu I\\
\frac{dR}{dt} & = & \gamma I - \mu R

\end{array}
$$

*Note:* We assume $\beta,\gamma,\mu > 0$.

Normalizing the system: (we divide the equations by N)

$$
\begin{array}{rcr} 

\frac{ds}{dt} & = & \mu - \beta s i - \mu s \\ 
\frac{ds}{dt} & = & \beta s i - \gamma i - \mu I\\
\frac{dr}{dt} & = & \gamma i - \mu r

\end{array}
$$

*Obs:* $s+i+r=1$.

The system of equations is now decoupled ($s$ and $i$ don't depend on $r$).

## Model Characteristics

* We have a first order autonomous system $\dot{z}=f(z,\xi)$ with $z=(s,i,r)^T\in[0,1]^3\in\mathbb{R}^3$

* The vector field $f$ that defines our system is a $C^\infty(\mathbb{R}^3)$ and thus Locally Lipschitz in every neighborhood contained in $\mathbb{R}^3$. We can then be sure the initial value problem $\dot{z}=f(z,\xi)$ with $z(0)=z_0$ has a unique solution.

* $f$ being $C^\infty(\mathbb{R}^3)$ also implies the flow is well defined for all $\mathbb{R}^3$.

* The third equation is decoupled.

## Equilibrium Points

Solving for equilibrium points we find two equilibria: $(1,0,0)$ and $\left(\frac{(\gamma + \mu)}{\beta},\frac{\mu}{(\gamma + \mu)}-\frac{\mu}{\beta},\frac{\gamma}{\gamma + \mu)}-\frac{\gamma}{\beta}\right)$

*Obs:* 

* $(1,0,0)$ doesn't depend on $\xi$.

* $\left(\frac{(\gamma + \mu)}{\beta},\frac{\mu}{(\gamma + \mu)}-\frac{\mu}{\beta},\frac{\gamma}{\gamma + \mu)}-\frac{\gamma}{\beta}\right)$ is in quadrant 1 as long as $\beta > \gamma + \mu$

* $\left(\frac{(\gamma + \mu)}{\beta},\frac{\mu}{(\gamma + \mu)}-\frac{\mu}{\beta},\frac{\gamma}{\gamma + \mu)}-\frac{\gamma}{\beta}\right)\rightarrow (1,0,0)$ as $\beta \rightarrow \gamma + \mu$

*Note:* We can rename $R_o=\frac{\beta}{\gamma + \mu}$.

## Local Dynamics

We begin by computing the jacobian of our system. 

$$
J=\begin{pmatrix}
-\beta i - \mu & -\beta s\\
\beta i & \beta s - \gamma - \mu
\end{pmatrix}
$$

### Local Dynamics for $(1,0,0)$

The associated jacobian is:

$$
\begin{pmatrix}
-\mu & -\beta\\
0 & \beta - \gamma - \mu
\end{pmatrix}
$$

The eigenvalues of our subsystem are:

* $\lambda_1=- \mu$
  
* $\lambda_2=\beta - \gamma - \mu$

There are three possible stability scenarios:

1. $\beta > \gamma + \mu \left(R_o>1\right)\rightarrow (\lambda_1<0,\lambda_2>0)$:  Saddle equilibrium.
   
2. $\beta=\gamma + \mu \left(R_o=1\right)\rightarrow (\lambda_1<0,\lambda_2=0)$:  Stable equilibrium.
   
3. $\beta<\gamma + \mu \left(R_o<1\right)\rightarrow (\lambda_1<0,\lambda_2<0)$:  Stable equilibrium.

### Local Dynamics for $\left(\frac{(\gamma + \mu)}{\beta},\frac{\mu}{(\gamma + \mu)}-\frac{\mu}{\beta},\frac{\gamma}{\gamma + \mu)}-\frac{\gamma}{\beta}\right)$

The associated jacobian is:

$$
\begin{pmatrix}
-\mu -\beta \left(-\frac{\mu}{\beta}+\frac{\mu}{\gamma + \mu}\right) & - \gamma - \mu\\
\beta \left(-\frac{\mu}{\beta}+\frac{\mu}{\gamma + \mu}\right) & 0
\end{pmatrix}
$$

The eigenvalues of our subsystem are:

* $\lambda_1=\frac{-\beta \mu-\sqrt{\beta^2\mu^2+4\mu\left(\gamma+\mu\right)^2\left(-\beta+(\gamma + \mu)\right)}}{2(\gamma + \mu)}$
  
* $\lambda_2=\frac{-\beta \mu+\sqrt{\beta^2\mu^2+4\mu\left(\gamma+\mu\right)^2\left(-\beta+(\gamma + \mu)\right)}}{2(\gamma + \mu)}$

There are three possible stability scenarios:

1. $\beta>\gamma + \mu \left(R_o>1\right)\rightarrow (\lambda_1<0,\lambda_2<0)$: Stable equilibrium.

2. $\beta=\gamma + \mu \left(R_o=1\right)\rightarrow (\lambda_1<0,\lambda_2=0)$: Stable equilibrium.

3. $\beta<\gamma + \mu \left(R_o<1\right)\rightarrow (\lambda_1<0,\lambda_2>0)$: Unstable equilibrium.

*Obs:* The eigenvalues $\lambda_1$ and $\lambda_2$ can be expressed in terms of $R_o$: 

* $\lambda_1=\frac{-R_o \mu-\sqrt{R_o^2\mu^2+4\mu\left(\gamma+\mu\right)\left(1-R_o\right)}}{2}$
  
* $\lambda_1=\frac{-R_o \mu+\sqrt{R_o^2\mu^2+4\mu\left(\gamma+\mu\right)\left(1-R_o\right)}}{2}$

## Nonlinear Dynamics

We define the following sets:

* $\mathbb{OCT}_1=\texttt{\{}(s,i,r)\in\mathbb{R}^3 | s,i,r > 0 \texttt{\}}$ is octant 1.

* $\mathbb{PL_{si}}=\texttt{\{}(s,i,r)\in\mathbb{R}^3 | s,i > 0, r = 0\texttt{\}}$ is the positive plane $s-i$.

* $\mathbb{PL_{sr}}=\texttt{\{}(s,i,r)\in\mathbb{R}^3 | s,r > 0, i = 0\texttt{\}}$ is the positive plane $s-r$.

* $\mathbb{AX_{s}}=\texttt{\{}(s,i,r)\in\mathbb{R}^3 | i,r > 0, s = 0\texttt{\}}$ is the positive plane $i-r$.

* $\mathbb{AX_{i}}=\texttt{\{}(s,i,r)\in\mathbb{R}^3 | s,r = 0, i > 0\texttt{\}}$ is the positive axis $i$.

* $\mathbb{AX_{r}}=\texttt{\{}(s,i,r)\in\mathbb{R}^3 | s,i = 0, r > 0\texttt{\}}$ is the positive axis $r$.


*Note:*

1. $\mathbb{OCT}_1$ is positively invariant.

2. The vector field defined by our system is transversal to the sets $\mathbb{PL_{si}}$ and $\mathbb{PL_{ir}}$

3. $\mathbb{PL_{sr}}$ is invariant.

4. $\mathbb{AX_{s}}$ is invariant.

5. The vector field defined by our system is transversal to the sets $\mathbb{AX_{i}}$ and $\mathbb{AX_{r}}$.


By Hartman-Grobman, every case in which the equilibria are of type hyperbolic ($\beta\neq\gamma+\mu,R_o\neq 1$), the linear dynamics are representative of the nonlinear dynamics as well.

### Non hyperbolic Case $\left(\beta = \gamma+\mu\right),R_o=1$:

Since $\mathbb{PL_{sr}}$ and $\mathbb{AX_{s}}$ are invariant, all that is left to do is determine whether the point $(1,0,0)$ is stable or not in the set $\mathbb{PL_{si}}$. This can be determined using the Poincare-Bendixon theorem in the following fashion:

**Global Dynamics**

We can assert that the simply connected set $\mathbb{PL_{si}}$ contains no periodic orbits, nor closed orbits, because applying the Bendixon-Dulac theorem on the this set with the function $\Phi(s,i)=s^ai^b$, $a=0,b=1$, the following is true:

$$
div(\Phi(s,i)(\mu-\beta s i - \mu s,\beta s i - \gamma i - \mu i)) < 0
$$

**Nullclines**

These are those curves that define those points of velocity zero:

* $s = \frac{\gamma + \mu}{\beta}$

* $i = \frac{\mu - \mu s}{\beta s}$

These curves define 4 subsets in the set $\mathbb{PL_{si}}$:

* $\mathbb{A}_1=\texttt{\{}(s,i)\in\mathbb{R}^2 | s>\frac{\gamma+\mu}{\beta},i>\frac{\mu-\mu s}{\beta s},1>s>0, 1>i>0\texttt{\}}$

* $\mathbb{A}_2=\texttt{\{}(s,i)\in\mathbb{R}^2 | s<\frac{\gamma+\mu}{\beta},i>\frac{\mu-\mu s}{\beta s},1>s>0, 1>i>0\texttt{\}}$

* $\mathbb{A}_3=\texttt{\{}(s,i)\in\mathbb{R}^2 | s<\frac{\gamma+\mu}{\beta},i<\frac{\mu-\mu s}{\beta s},1>s>0, 1>i>0\texttt{\}}$

* $\mathbb{A}_4=\texttt{\{}(s,i)\in\mathbb{R}^2 | s>\frac{\gamma+\mu}{\beta},i<\frac{\mu-\mu s}{\beta s},1>s>0, 1>i>0\texttt{\}}$


We define the set $\mathbb{B}=\mathbb{L}^º$, with $\mathbb{L}=\mathbb{L}_1\cup\mathbb{L}_2\cup\mathbb{L}_3\cup\mathbb{L}_4\cup\mathbb{L}_5$ being a closed curve.

* $\mathbb{L}_1=\texttt{\{}\rho\left(t;\left(\frac{\beta}{\gamma + \mu},i_o\right)\right)\in \mathbb{R}^2 | \rho\left(t_{-1};\left(\frac{\beta}{\gamma + \mu},i_o\right)\right)=(0,i_{-1}),t\in[t_{-1}t_{0}]\texttt{\}}$

* $\mathbb{L}_2=\texttt{\{}(s,i)\in\mathbb{R}^2 | i=i_o, s\in\left[\frac{\beta}{\gamma + \mu},\frac{\mu}{i_o\beta+\mu}\right]\texttt{\}}$

* $\mathbb{L}_3=\texttt{\{}(s,i)\in\mathbb{R}^2 | s=\frac{\mu}{i_o\beta + \mu}_o, i\in\left[i_o,i_1\right]\texttt{\}}$

* $\mathbb{L}_4=\texttt{\{}\rho\left(t;\left(\frac{\mu}{i_o\beta + \mu},i_o\right)\right)\in \mathbb{R}^2 | \rho\left(t_{1};\left(\frac{\mu}{i_o\beta + \mu},i_o\right)\right)=\left(\frac{\beta}{\gamma+\mu},i_2\right),t\in[t_{0}t_{1}]\texttt{\}}$

* $\mathbb{L}_5=\texttt{\{}(s,i)\in\mathbb{R}^2 | i=i_2, s\in\left[0,\frac{\beta}{\gamma+\mu}\right]\texttt{\}}$

with $i_0\in\left(0,\frac{\gamma+\mu}{\beta},i_q\in\left(\right(\frac{\gamma+\mu}{\beta},K\right)$

Set $\mathbb{B}$ contains an equilibrium point in the hyperbolic case $\beta>\gamma+\mu \space \space \space (R_o>1)$. The Poincare- Bendixon theorem tells us that every orbit that starts in $\mathbb{B}$ will eventually end up in said equilibrium point.

In the non hyperbolic case, observing the vector field defined by $\mathbb{A}_1$, $\mathbb{A}_2$ and $\mathbb{A}_3$ we can see that every orbit that has its origin in $\mathbb{A}_3$, later passes through $\mathbb{A}_2$ which in turn later passes through $\mathbb{A}_1$, crossing the nullclines transversally. Finally, every orbit in $\mathbb{A}_1$ es stable, therefore, in this case, the equilibrium $(1,0)$ is asymptotically stable with resepect to $\mathbb{PL}_{si}$.

In the hyperbolic case $\beta<\gamma+\mu \space \space \space (R_o<1)$ $\mathbb{B}$ preserves the linear stability of the point $(1,0)$, that is, it is  asymptotically stable. In particular, every orbit that starts in $\mathbb{PL_{si}}$ will end up in this equilibrium.


**This is the end of the mathematical anaylsis of the model, the arguably more important model interpetation is left as an exercise to the reader.**

There are 2 primary cases, depending on the value of $R_o$:

1. Case $R_o\leq 1$
    
2. Case $R_o> 1$