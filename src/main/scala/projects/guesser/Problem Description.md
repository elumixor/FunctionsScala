# Guesser 

## Definition:
Guess a number in range `[-1, 1]` 

### Task:
We try to guess a generated number **using reinforcement learning.** 
When making a guess we then see if the guess was better or worse than the previous one.
We then make an adjusted guess.

```guess = xt```

Mathemmaticaly, this could be written as.

### Inputs:
> Known data and their description, that is used 
in mode creation and training to generate outputs

### Outputs:
> What model + inputs produces in the end. 
Also how the model should look

## Solution ideas:
* Solved using gradient descent approach 
    > This is not a valid solution, regarding reinforcement
    learning and sort of defeats the purpose of guesser as
    it directly uses the knowledge of target's value to get
    closer towards it.
    
    ```scala
    val target: Double = lib.p5.random(-1, 1)  
    var guess: Double = lib.p5.random(-1, 1)        
  
    // using target here is not what we want for reinforcement learning
    val MSE = lib.math.functions.Function("((target - guess)/2)^2")(Map("target" -> target))
 
    val derived = MSE.derivative("guess")
    val learningRate = .1
    
    /** Approach function */
    def approximate(): Unit = {
      // Compute gradient descent
      val descent = derived(guess).value.get
      
      // Approximate guess
      guess -= learningRate * descent
    }
    ```  

* Similar and different
    >  To use reinforcement, it seems like we need to
    introduce concepts of similarity.
    >
    > In terms of numbers, difference between `x` and
    `y` is just `|x - y|`
    >
    > The bigger the difference, obviously, the 
    smaller the similarity is.
    
* Generating similar numbers
    > Similar number means a number, that does not differ ''a lot'' from a reference.
     More precisely, similarity is a function that displays a difference between numbers.
    >
    > **Divergence** is the maximum possible difference between randomly generated number
     and a reference number. It defines a range generated numbers as:
     `(reference - divergence, reference + divergence)`
    > 
    > By specifying divergence and a reference we can generate similar numbers:
    >
    > similar = reference + signum(R - 0.5) * divergence * R
    >
    > where `R` is randomly generated number in (0, 1) and  `signum` is a
    [sign function](https://en.wikipedia.org/wiki/Sign_function).
    >
    > However, we would like to **more likely** generate a number, that is more similar
    to reference. This can be done by specifying similarity:
    >
    > similar = reference * signum(R - 0.5) * divergence * R ^**similarity**^
    >
    > The bigger the similarity is, the more likely a generated number will be more
    similar to a reference.
    >
    > As similarity approaches +infinity, the difference between a generated number and a
    reference approaches 0. And the likelihood of generating such number approaches 100%
    >
    > As similarity approaches -infinity, the difference between a generated number and a
    reference approaches infinity. And the likelihood of generating such number approaches 0%
    >
    > If similarity equals 0, then we "don't care" about the similarity at all - only about
    divergence, thus randomly picking numbers in allowed  range
    >
    > We also want to map similarity to ange -1, 1, so we use tan(similarity * pi / 2) for that 

* Guess's gain (how good the value is)
    > We need to determine how good the guess is.
    [MSE](https://en.wikipedia.org/wiki/Mean_squared_error)
    seems like a good way to assign gain.
    
* Exploring (not getting stuck)
    > We need to attempt to make better guesses. This
    means exploring new options, even if they might 
    bring worse results.
    
* Probabilistic output
    > What we are looking for is a probabilistic model:
    it shows **how good a guess** is likely to be.  
    
* Approach:
    1. Make a `guess`
    2. Assign its weight using `MSE` function
    3. If the guess was *good*, make a guess,
    that is similar.
 
    > Statistically, we get closer towards the goal.
 
    ```scala
    val target: Double = lib.p5.random(-1, 1)  
    var guess: Double = lib.p5.random(-1, 1)    
  
    val MSE = lib.math.functions.Function("((target - guess)/2)^2")(Map("target" -> target))

    
    /** How good a guess is */
    def gain(guess: Double): Double = 1.0 - MSE(guess).value.get

    /** Generate a similar value */
    def similar(n: Double, maxDifference: Double): Double = {
      val g: Double = lib.p5.random(0, 1)
      val sign: Double = if (lib.p5.random(0, 1) >= 0.5) 1 else -1
  
      lib.p5.map(g, 0, 1, n, n + maxDifference * sign)
    }
  
    /** Approach function */
    def approximate(): Double = {
      var s = 0.0
      do { s = similar(guess, 1.0  -  gain(guess)) } while (s < -1 || s  > 1)
      guess = s
      guess
    }
    ```
    
    > The downside is that approximation speed lowers
    drastically as we approach the target, so we need
    more variability.

* Take previous experience into account:
    > When we guessed better, we should make guesses
    more like the best bets, and less like worse. We
    should take into account previous guesses - the
    example above always generates new guess based
    upon **only the last guess**
    
* Similarity function
    > Todo    

* Similar to multiple values 
    > ???
    
    * Weighted 
  
 