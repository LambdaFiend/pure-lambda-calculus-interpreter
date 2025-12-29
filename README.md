# ImpLam
Impure Lambda (as of yet, it's totally pure!)

Run ghci Main.hs

Then write "main"  and enter.

Or...

Run ghc Main.hs

Then run Main (./Main, for example)

The interface should be intuitive enough, although totally unappealing.

The interface will be grealy improved sometime.

Currently, the only form of reducing is not the most efficient one (it runs the AST for each reduction!).

Soon, I'll add a chain reducing function (only traverses teh AST once).

I will also add a way of showing whether it was an eta or a beta reduction done for each step.

Many other improvements will be made, for example environment variables and libraries.
More control and flexibility for the interface. And so on...
