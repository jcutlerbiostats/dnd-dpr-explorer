---
tags:
  - dnd
  - dpr
  - probability
  - r
created: 2026-09-08
source: https://dnd5e.wikidot.com/barbarian
---
# Barbarian (Level 1) — Offensive & Defensive DPR

**Class:** Barbarian, level 1 (no subclass yet) · **Source:** [Barbarian](https://dnd5e.wikidot.com/barbarian)

This is the simplest possible version of the derivation in [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]]. At level 1 almost every complication is absent, so the two formulas collapse to a single (probability × damage) sum each.

Companion R script: `level1_barbarian_dpr.R` (same folder).

---
## 1. What a level 1 barbarian actually has

| Level | PB  | Rages / long rest | Rage damage | Features                |
| ----- | --- | ----------------- | ----------- | ----------------------- |
| 1     | +2  | 2                 | +2          | Rage, Unarmored Defense |

- **Rage** (bonus action) — while raging, +2 damage on melee weapon attacks that use Strength, and resistance (½ damage) to bludgeoning, piercing, and slashing. Lasts 1 minute; ends early if you haven't attacked or been attacked since your last turn. Two uses per long rest.
- **Unarmored Defense** — $AC = 10 + \text{DEX mod} + \text{CON mod}$ when not wearing armor (a shield is still allowed).

What is **not** there yet, and therefore drops out of the formulas:

| Feature | Arrives at | Effect on the math |
|---|---|---|
| Reckless Attack | 2 | no advantage on your attacks, no advantage for enemies → all rolls are flat |
| Primal Path (subclass) | 3 | no Divine Fury, no Bear resistance, no riders of any kind |
| Extra Attack | 5 | $A = 1$ attack per round |
| Brutal Critical | 9 | $B = 0$ extra crit dice |

---
## 2. Hit and crit probabilities (flat rolls)

With attack bonus $b$ and target $AC$, the minimum d20 roll is $n = AC - b$:

$$p_{\text{hit}} = \frac{21 - n}{20}, \qquad p_{\text{crit}} = \frac{1}{20} = 0.05, \qquad p_{\text{noncrit}} = p_{\text{hit}} - 0.05$$

(clamped so a natural 1 misses and a natural 20 hits). No advantage anywhere at this level, so these are the only probabilities needed on either side.

---
## 3. Offensive DPR

Weapon with $k$ dice averaging $\bar d$, Strength modifier $S$, rage damage $R$ (2 if raging, 0 if not), one attack:

$$D_{\text{hit}} = k\bar d + S + R, \qquad D_{\text{crit}} = D_{\text{hit}} + k\bar d$$

$$\boxed{\text{DPR}_{\text{off}} = p_{\text{noncrit}}\, D_{\text{hit}} + p_{\text{crit}}\, D_{\text{crit}}}$$

### Worked example — greataxe, STR 16, raging, vs. AC 13

| Variable | Value |
|---|---|
| STR 16 → $S$ | +3 |
| Proficiency bonus | +2 → attack bonus $b = 5$ |
| Rage damage $R$ | +2 |
| Weapon | greataxe, $k = 1$, d12, $\bar d = 6.5$ |
| Target AC | 13 → need $n = 8$ |

1. $p_{\text{hit}} = 13/20 = 0.65$; $p_{\text{crit}} = 0.05$; $p_{\text{noncrit}} = 0.60$.
2. $D_{\text{hit}} = 6.5 + 3 + 2 = 11.5$; $D_{\text{crit}} = 11.5 + 6.5 = 18$.
3. $\text{DPR}_{\text{off}} = 0.60 \times 11.5 + 0.05 \times 18 = 6.9 + 0.9 = \mathbf{7.8}$

Not raging ($R = 0$): $0.60 \times 9.5 + 0.05 \times 16 = \mathbf{6.5}$. Rage is worth $p_{\text{hit}} \times 2 = 0.65 \times 2 = 1.3$ DPR at this level, which is exactly the +2 rider times the chance to land it.

---
## 4. Defensive DPR (expected damage taken per round)

Enemy with attack bonus $b_e$, $A_e$ attacks, hit damage $E_{\text{hit}}$ whose dice average $E_{\text{dice}}$. Probabilities from §2 using the barbarian's AC as the target:

$$\text{Raw} = A_e \left[ p_{\text{noncrit}}\, E_{\text{hit}} + p_{\text{crit}}\,(E_{\text{hit}} + E_{\text{dice}}) \right]$$

$$
m = \begin{cases} \tfrac{1}{2} & \text{raging and damage is bludgeoning/piercing/slashing} \\ 1 & \text{otherwise} \end{cases}
\qquad
\boxed{\text{DPR}_{\text{def}} = m \cdot \text{Raw}}
$$

No subclass yet, so the only source of resistance is Rage itself.

### Worked example — same barbarian vs. two attacks at +4, 1d6+2 slashing

| Variable                 | Value                                          |
| ------------------------ | ---------------------------------------------- |
| Barbarian AC             | $10 + 2\ (\text{DEX}) + 2\ (\text{CON}) = 14$  |
| Enemy attack bonus $b_e$ | +4 → need $n = 10$                             |
| Enemy attacks $A_e$      | 2                                              |
| Enemy damage             | $1d6 + 2$ → $E_{\text{hit}} = 5.5$, crit $= 9$ |

1. $p_{\text{hit}} = 11/20 = 0.55$; $p_{\text{crit}} = 0.05$; $p_{\text{noncrit}} = 0.50$.
2. $\text{Raw} = 2\,[0.50 \times 5.5 + 0.05 \times 9] = 2\,[2.75 + 0.45] = 6.4$.
3. Raging, slashing → $m = \tfrac{1}{2}$: $\text{DPR}_{\text{def}} = \mathbf{3.2}$ damage taken per round.

| Scenario | Raw | $m$ | Damage taken / round |
|---|---|---|---|
| Raging, slashing | 6.4 | ½ | **3.2** |
| Not raging, slashing | 6.4 | 1 | 6.4 |
| Raging, fire | 6.4 | 1 | 6.4 |

---
## 5. Why this is the baseline

Everything the class gains later is a modification of one of these two boxed sums:

- Reckless Attack (lvl 2) swaps the flat $p$'s for advantage $p$'s on **both** sides.
- A subclass (lvl 3) adds a rider term to $\text{DPR}_{\text{off}}$ (Zealot) or a case to $m$ (Bear Totem).
- Extra Attack (lvl 5) multiplies the offensive sum by $A = 2$.
- Brutal Critical (lvl 9+) enlarges $D_{\text{crit}}$ only.

See the Zealot note for all four layered on at once.

---
## Related
- [[Barbarian (Path of the Zealot) — Offensive & Defensive DPR]]
- `level1_barbarian_dpr.R`
- [[Probability]] · [[Expected value]]
