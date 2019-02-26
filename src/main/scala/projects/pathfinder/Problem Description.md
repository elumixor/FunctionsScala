# Pathfinder

## Definition
We have a `pathfinder`, that tries to get to his `target`

### Task:
1. Get to the `target` **as efficiently as possible**

    > Solution would be to move in a straight line 
    towards the target.

### Inputs:
1. **Location** of his `target`
2. His own **location**

### Outputs:
1. Steering vector

   > In this problem we disregard velocity and 
   limit  ourselves to 2D space for simplicity.
   >
   > This means that steering vector is a simple
   2D normalized vector. 

## Solution ideas:
* Observation:
    1. `Pathfinder` makes a random guess about how 
    should it steer.
    2. It observes whether it has brought it 
    closer to the target or not.
    3. It records the guess and its result.
    4. It makes a guess, that is similar to 
    the best previous guesses.
    5. Repeat steps 2-4 until convergence.
    
* More-like-normal-algorithm
    1. Calculate the **distance** to the `target`.
    2. Make a random guess and steer.
    3. Calculate the distance
