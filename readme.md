# Information of developer
    1. Name: Seongyong Yoon
    2. Affiliation: Max-Planck-Institut für Eisenforschung GmbH
    3. E-mail: s.yoon@mpie.de

# Brief description
    - An universial numerical differentiation tool (single/multi variate) is shared here. (Language: Fortran)
    - Numerical differentiation is operated using (1) Finite difference method (Real number)/ (2) Complex number / (3) Hyper-Dual number.
    - Particularly, Hyper-dual number gives rise to ZERO truncation errors.
    - Hyper-dual number is implemented after modifying DNAD module.
    - Formulation and basic algebra are detailed in Document folder.

# Original source of Dual number module
    https://github.com/joddlehod/dnad

# Reference
    [1] Fike - Ph.D Thesis
    [2] Fike, J., & Alonso, J. (2011, January). The development of hyper-dual numbers for exact second-derivative calculations. In 49th AIAA aerospace sciences meeting including the new horizons forum and aerospace exposition (p. 886).
    [3] Fohrmeister, V., Bartels, A., & Mosler, J. (2018). Variational updates for thermomechanically coupled gradient-enhanced elastoplasticity—implementation based on hyper-dual numbers. Computer Methods in Applied Mechanics and Engineering, 339, 239-261.
    [4] Zhou, X., Shi, A., Lu, D., Chen, Y., Zhuang, X., Lu, X., & Du, X. (2023). A return mapping algorithm based on the hyper dual step derivative approximation for elastoplastic models. Computer Methods in Applied Mechanics and Engineering, 417, 116418.
    [5] Lai, K. L., & Crassidis, J. (2008). Extensions of the first and second complex-step derivative approximations. Journal of Computational and Applied Mathematics, 219(1), 276-293.
    [6] Yu, W., & Blair, M. (2013). DNAD, a simple tool for automatic differentiation of Fortran codes using dual numbers. Computer Physics Communications, 184(5), 1446-1452.

