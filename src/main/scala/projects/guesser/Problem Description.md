# Guesser 

## Definition:
Guess a number in range `[-1, 1]` 

### Task:
We try to guess a generated number **using reinforcement learning.** 
When making a guess we then see if the guess was better or worse than the previous one.
We then make an adjusted guess.

```guess = xt```

Mathemmatically, this could be wrritten as.

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
    val MSE = lib.Function("(target - guess)^2")  
    
    val derived = MSE.derivative("guess")
    val learningRate = .1
    
    /** Approach function */
    def approximate(): Unit = {
      // Compute gradient descent
      val descent = derived(Map("target" -> target, "guess" -> guess)).value.get
      
      // Approximate guess
      guess -= learningRate * descent
    }
    ```  

    ddfsdfdkfdj
