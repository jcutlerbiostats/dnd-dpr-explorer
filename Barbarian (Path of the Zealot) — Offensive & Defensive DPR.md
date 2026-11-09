---
tags:
  - dnd
  - dpr
  - probability
  - r
created: 2026-09-08
source: https://dnd5e.wikidot.com/barbarian
---
# Barbarian (Path of the Zealot) — Offensive & Defensive DPR

**Class:** Barbarian · **Subclass:** Path of the Zealot · **Source:** [Barbarian](https://dnd5e.wikidot.com/barbarian), [Path of the Zealot](https://dnd5e.wikidot.com/barbarian:zealot)

DPR = *damage per round*, an expected value. Offensive DPR is the expected damage the barbarian deals in one round. Defensive DPR (as defined here) is the expected damage the barbarian *takes* in one round from a given enemy. Both are just sums of (probability × damage) over the dice outcomes that can happen.

Companion R script: `zealot_dpr.R` (same folder).

---
## 1. Class inputs from the source table

| Level | PB | Rages | Rage damage | Relevant features |
|---|---|---|---|---|
| 1 | +2 | 2 | +2 | Rage, Unarmored Defense |
| 2 | +2 | 2 | +2 | Reckless Attack, Danger Sense |
| 3 | +2 | 3 | +2 | Primal Path → **Divine Fury** (Zealot) |
| 5 | +3 | 3 | +2 | **Extra Attack** (2 attacks per Attack action) |
| 9 | +4 | 4 | +3 | Brutal Critical (1 die) |
| 13 | +5 | 5 | +3 | Brutal Critical (2 dice) |
| 16 | +5 | 5 | +4 | — |
| 17 | +6 | 6 | +4 | Brutal Critical (3 dice) |
| 20 | +6 | Unlimited | +4 | Primal Champion (STR/CON +4, max 24) |

Features that enter the formulas:

- **Rage** — bonus melee damage (the "rage damage" column) on Strength-based attacks; resistance (½) to bludgeoning, piercing, and slashing damage while raging. *Rages per long rest* limits how many combats you get this; it does not change per-round DPR while raging.
- **Reckless Attack** — you attack with **advantage** on melee STR attacks this turn; enemies get **advantage** against you until your next turn. This is the one feature that couples offense and defense.
- **Extra Attack** (5th) — 2 weapon attacks per Attack action.
- **Brutal Critical** (9/13/17) — 1/2/3 extra weapon dice on a crit.
- **Divine Fury** (Zealot 3rd) — the *first* creature you hit each turn with a weapon attack takes an extra $1d6 + \lfloor \text{level}/2 \rfloor$ radiant or necrotic damage. Once per turn, so it is added outside the per-attack sum.
- **Unarmored Defense** — $AC = 10 + \text{DEX mod} + \text{CON mod}$.

---
## 2. Hit and crit probabilities

Let $b$ = attack bonus (STR mod + PB) and $AC$ = target armor class. The minimum d20 roll needed is $n = AC - b$.

$$p_{\text{hit}} = \frac{21 - n}{20}, \qquad p_{\text{crit}} = \frac{1}{20}$$

(clamped so a natural 1 always misses and a natural 20 always hits). With **advantage** (Reckless Attack), you roll twice and keep the higher, so you only miss if *both* rolls miss:

$$p^{\text{adv}}_{\text{hit}} = 1 - (1 - p_{\text{hit}})^2, \qquad p^{\text{adv}}_{\text{crit}} = 1 - \left(\tfrac{19}{20}\right)^2 = 0.0975$$

The non-crit hit probability is $p_{\text{hit}} - p_{\text{crit}}$ in either case.

---
## 3. Offensive DPR formula

For a weapon with $k$ damage dice of average $\bar d$ (a d12 averages 6.5), STR modifier $S$, rage damage $R$, Brutal Critical dice $B$, and $A$ attacks per round:

$$D_{\text{hit}} = k\bar d + S + R$$
$$D_{\text{crit}} = D_{\text{hit}} + (k + B)\,\bar d \quad \text{(only the dice double; modifiers do not)}$$

$$\text{DPR}_{\text{weapon}} = A \left[ (p_{\text{hit}} - p_{\text{crit}})\, D_{\text{hit}} + p_{\text{crit}}\, D_{\text{crit}} \right]$$

**Divine Fury** rides on the first hit of the turn. The probability at least one of $A$ attacks hits is $1 - (1 - p_{\text{hit}})^A$, and conditional on a hit the chance it was a crit is $p_{\text{crit}}/p_{\text{hit}}$ (the d6 doubles on a crit; the flat $\lfloor L/2 \rfloor$ does not):

$$\text{DF} = \left[1 - (1 - p_{\text{hit}})^A\right] \left[ \left(1 - \tfrac{p_{\text{crit}}}{p_{\text{hit}}}\right)(3.5 + \lfloor L/2 \rfloor) + \tfrac{p_{\text{crit}}}{p_{\text{hit}}}(7 + \lfloor L/2 \rfloor) \right]$$

$$\boxed{\text{DPR}_{\text{off}} = \text{DPR}_{\text{weapon}} + \text{DF}}$$

### Worked example — level 5 Zealot, greataxe, raging, Reckless

| Variable | Value |
|---|---|
| Level $L$ | 5 |
| STR 18 → $S$ | +4 |
| Proficiency bonus | +3 → attack bonus $b = 7$ |
| Rage damage $R$ | +2 |
| Attacks $A$ | 2 (Extra Attack) |
| Weapon | greataxe, $k=1$, d12, $\bar d = 6.5$ |
| Brutal Critical $B$ | 0 |
| Divine Fury | $1d6 + 2$ |
| Target AC | 15 → need $n = 8$ |

1. $p_{\text{hit}} = 13/20 = 0.65$ flat → with advantage $1 - 0.35^2 = 0.8775$; $p_{\text{crit}} = 0.0975$; non-crit hit $= 0.78$.
2. $D_{\text{hit}} = 6.5 + 4 + 2 = 12.5$; $D_{\text{crit}} = 12.5 + 6.5 = 19$.
3. $\text{DPR}_{\text{weapon}} = 2\,[0.78 \times 12.5 + 0.0975 \times 19] = 2\,[9.75 + 1.8525] = 23.21$.
4. $\text{DF} = [1 - 0.1225^2]\,[0.889 \times 5.5 + 0.111 \times 9] = 0.985 \times 5.889 = 5.80$.

$$\text{DPR}_{\text{off}} \approx 23.2 + 5.8 = \mathbf{29.0}$$

For comparison, the same build **without** Reckless Attack ($p_{\text{hit}} = 0.65$, $p_{\text{crit}} = 0.05$): weapon $= 2[0.60 \times 12.5 + 0.05 \times 19] = 16.9$, DF $\approx 4.9$, total ≈ **21.8**. Reckless is worth about +7 DPR here.

Same build at level 9 (STR 20, PB +4, rage +3, Brutal Critical 1 die, Divine Fury $1d6+4$): **≈ 37.6**.

---
## 4. Defensive DPR formula (expected damage taken per round)

Let the enemy have attack bonus $b_e$, $A_e$ attacks per round, and hit damage $E_{\text{hit}}$ with dice averaging $E_{\text{dice}}$. The barbarian's AC comes from Unarmored Defense. Compute $p_{\text{hit}}, p_{\text{crit}}$ exactly as in §2 but from the enemy's side, using the **advantage** formulas if the barbarian used Reckless Attack.

$$\text{Raw} = A_e \left[ (p_{\text{hit}} - p_{\text{crit}})\, E_{\text{hit}} + p_{\text{crit}}\,(E_{\text{hit}} + E_{\text{dice}}) \right]$$

Then apply the resistance multiplier $m$:

$$m = \begin{cases} \tfrac{1}{2} & \text{raging and damage is bludgeoning/piercing/slashing} \\ \tfrac{1}{2} & \text{raging Bear Totem and damage is anything but psychic} \\ 1 & \text{otherwise} \end{cases}$$

$$\boxed{\text{DPR}_{\text{def}} = m \cdot \text{Raw}}$$

(A Zealot gets no extra damage resistance; its 14th-level Rage Beyond Death changes *when you drop*, not how much you take. The Bear row is included to show how the subclass choice enters this formula.)

### Worked example — same level 5 Zealot vs. a two-attack enemy

| Variable | Value |
|---|---|
| Barbarian AC | $10 + 2\ (\text{DEX}) + 3\ (\text{CON}) = 15$ |
| Enemy attack bonus $b_e$ | +6 → need $n = 9$ |
| Enemy attacks $A_e$ | 2 |
| Enemy damage | $1d8 + 4$ slashing → $E_{\text{hit}} = 8.5$, crit $= 13$ |
| Barbarian used Reckless? | yes → enemy has advantage |
| Raging? | yes → $m = \tfrac{1}{2}$ |

1. $p_{\text{hit}} = 12/20 = 0.60$ flat → advantage $1 - 0.4^2 = 0.84$; $p_{\text{crit}} = 0.0975$; non-crit $= 0.7425$.
2. $\text{Raw} = 2\,[0.7425 \times 8.5 + 0.0975 \times 13] = 2\,[6.311 + 1.268] = 15.16$.
3. $\text{DPR}_{\text{def}} = 0.5 \times 15.16 = \mathbf{7.58}$ damage taken per round.

Variants, same enemy:

| Scenario | Raw | $m$ | Damage taken / round |
|---|---|---|---|
| Reckless + raging, slashing | 15.16 | ½ | **7.58** |
| No Reckless, raging, slashing | 10.65 | ½ | 5.33 |
| Reckless + raging, **fire** (Zealot) | 15.16 | 1 | 15.16 |
| Reckless + raging, fire (**Bear Totem**) | 15.16 | ½ | 7.58 |

So at level 5, Reckless Attack buys ≈ +7.2 offensive DPR at a cost of ≈ +2.3 damage taken per round against this enemy. That trade is the reason barbarians almost always Reckless while raging.

---
## Related
- `zealot_dpr.R` — functions `hit_probs()`, `zealot_offense()`, `zealot_defense()` reproduce every number above
- [[Probability]] · [[Expected value]]
