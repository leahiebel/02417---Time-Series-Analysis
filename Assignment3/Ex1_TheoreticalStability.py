import numpy as np

a = 1
b = -0.75
c = -0.3

discriminant = b**2 - 4*a*c
roots = np.roots([a, b, c])
modulus = np.abs(roots)

print("Discriminant:", np.round(discriminant,2))
print("Roots:", np.round(roots, 2))
print("Modulus:", np.round(modulus, 2))
