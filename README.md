# Table of Contents

* Table of Contents
* Why?
* Naming conventions
* How do I...
    * ...use the library to write monadic actions?
    * ...use a new transformer with existing kinds of effects?
    * ...create an effect stack for a new kind of effect?
* Why not...
    * ...type-based resolution?
    * ...type-level tags?

# Why?

The `transformers` package gives us a nice library for building up monad
transformer stacks that combine just the right mix of effects for a given
application. For example, in a compiler, we might want to combine `IO` effects
for reading and writing files, a scoping environment effect, the ability to
report warnings and errors, state for generating fresh variable names or
tracking type unification information, and so on. We might cook up a quite
complicated stack like

    type Compiler = ReaderT ScopingInformation
                  ( StateT UnificationState
                  ( ExceptT TypeError
                  ( WriterT [Warning]
                  ( StateT FreshNameGenerator
                  ( IO
                  )))))

to capture all of these effects in one monad.

With just the tools provided by the `transformers` package, though, this type
can be somewhat frustrating to use. Getting access to the `WriterT [Warning]`
part of the stack, for example, involves lifting the write effect through three
layers of the stack using something like:

    warn :: Warning -> Compiler ()
    warn w = lift (lift (lift (tell [w])))

Besides being tedious, this also calcifies the monad stack; if later we
discover we got the stack in the wrong order, or decide we need to add another
effect, we would need to revisit all the places where we did such lifting and
reconsider exactly how many occurrences of `lift` there should be.

The `mtl` package addresses this problem by adding one typeclass for each kind
of effect. (Transformers which don't provide that effect pass it through.) This
gives us two benefits:

1. We can write type signatures that constrict us to using fewer effects than
   our top-level application monad actually provides, and get the compiler to
   check that we have really used only those effects. These actions can still
   be used in the richer top-level application monad.
2. The need for explicitly `lift`ing is drastically reduced, with the typeclass
   resolution mechanism inferring the correct number of `lift`s for us.

For example, using that library, we might write

    warn :: MonadWriter [Warning] m => m ()
    warn w = tell [w]

which now continues to work even if we change the monad stack later, provided
we retain the property of having just one `WriterT` in the stack. The three
uses of `lift` are inferred, and will be adjusted up or down as needed as the
top-level application monad changes.

However, if one wishes to have two or more copies of a single kind of effect,
there is no convenient, generic way to choose anything other than the one that
appears topmost in the stack. With our `Compiler` monad above, for example, we
might write

    unify :: MonadState UnificationState m => Type -> Type -> m ()
    unify t1 t2 = get >>= \us -> ...

to get access to the unification state. But if we want to access the
`FreshNameGenerator`, we are back to writing fragile `lift`-based code:

    freshName :: Compiler Name
    freshName = lift (lift (lift (lift (modify (...)))))

We can, with some effort and perhaps a confusing type signature, retain some of
the benefits of indicating in the type exactly which effects are used by mixing
`mtl`-style actions with `transformers`-style `lift`ing, though I dare say this
style is as yet not very popular:

    freshName :: (MonadTrans t1, MonadTrans t2, MonadState FreshNameGenerator m) => t1 (t2 m) Name
    freshName = lift (lift (modify (...)))

The fragility of `lift` remains, though.

The `effect-stack` package addresses this problem, providing a way to choose
lower layers of the monad stack generically and without explicitly writing the
correct number of `lift`s. It introduces a separate stack for each kind of
effect, and provides an operation for popping one layer of a given effect's
stack. For example, we can still write

    unify :: MonadState UnificationState m => Type -> Type -> m ()

for actions that access the topmost state, but with this library we can also write

    freshName :: (StateStack m, MonadState FreshNameGenerator (PopState m)) => m Name

to access the state from underneath the outermost `StateT`, no matter how deep
it is. We can implement this type using `liftState`; for example:

    freshName = liftState (modify (...))

The typeclass resolution mechanism will turn `liftState` into the correct
number of `lift`s to get from one `StateT` to the next.

Our `Compiler` monad has only two kinds of state, but one could imagine needing
a third. Writing down the type for accessing the third type shows that using the
primitive `StateStack` and `PopState` operations quickly becomes tedious with
deep stacks:

    thirdStateGet :: (StateStack m, StateStack (PopState m), MonadState X (PopState (PopState m))) => m X
    thirdStateGet = liftState (liftState get)

Consequently, the library also provides some type families and operations that
ease this iteration. Using them, we can also write `thirdStateGet` this way:

    thirdStateGet :: MonadStateDepth 2 m X => m X
    thirdStateGet = depthState @2 get

Of course, and unfortunately, inferred types will still use the fully-expanded
form, but at least the human-written types can be a bit prettier.

# Naming conventions

There is one module per kind of effect, named `Control.Monad.Stack.<Effect>`.
Generally, if there is a class for the effect, we drop the initial `Monad` from
the class name and use that as the name of the effect (e.g. `MonadState` &rarr;
`State`). Otherwise we use the final part of the module name from
`transformers` as the effect name (e.g. `Control.Monad.Trans.Accum` &rarr;
`Accum`). Each module exports the following things:

* A typeclass for popping one layer of that kind of effect off the stack at a
  time. This should generally be viewed as a low-level tool, but it may also be
  independently useful.
    * The class is named `<Effect>Stack`.
    * There is an associated type family `Pop<Effect>`; it takes a monad, and
      removes enough transformers to drop the outermost transformer of the
      current kind of effect. For example, `PopState Compiler` would throw away
      the outermost `ReaderT` and `StateT`, leaving a new stack that began at
      the `ExceptT`.
    * There is a method `lift<Effect>`; it applies `lift` the appropriate
      number of times to take an action one layer down in the effect stack and
      lift it to the full monad.
* A type alias `<Effect>Depth`. It takes a type-level number and a monad, and
  calls `Pop<Effect>` the given number of times on the monad. This should
  probably also be considered a low-level tool.
* A type alias `<Effect>Constraints`. It takes a type-level number and a monad,
  and produces a constraint saying that you are permitted to call `Pop<Effect>`
  and `lift<Effect>` the given number of times with the monad. For transformers
  with no associated class, this will likely be the most commonly-used
  type-level export.
* A function `depth<Effect>`. It takes a type-level number and a monadic
  action, and calls `lift<Effect>` the given number of times on the action.
  This will most likely be the most commonly-used computation-level export.

For effects that are associated with a class, the module will also export:

* A type alias `Monad<Effect>Depth`. It takes a type-level number and a monad
  as its first two arguments. For classes which have other parameters than the
  monad, those parameters follow in the same order that the `Monad<Effect>`
  class demands them. (But note that the monad always comes before the other
  arguments, unlike in `mtl`!) It produces a constraint saying that you can
  call `Pop<Effect>` and `lift<Effect>` the given number of times with the
  monad, and that if you call `Pop<Effect>` the given number of times then the
  result is an instance of `Monad<Effect> <args>`. This will likely be the most
  commonly-used type-level export when it is available.

# How do I...

## ...use the library to write monadic actions?

Generally, you will mix `mtl`-style classes for identifying what effect you
want and `effect-stack`-style classes for identifying which layer of your
transformer should provide that effect. In what follows, we will recap the
`Compiler` example from the "Why?" section, including a complete, compilable
file demonstrating simple usage of `effect-stack`. First some imports, data
declarations, and other standard-ish nonsense:

    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE TypeApplications #-}

    -- base
    import System.Exit

    -- mtl
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Writer
    import Control.Monad.Except

    -- effect-stack
    import Control.Monad.Stack.State
    import Control.Monad.Stack.Error

    type Compiler = ReaderT ScopingInformation
                  ( StateT UnificationState
                  ( ExceptT TypeError
                  ( WriterT [Warning]
                  ( StateT FreshNameGenerator
                  ( IO
                  )))))

    type Name = String
    type ScopingInformation = [Name]
    type UnificationState = ()
    type Type = ()
    type TypeError = (Type, Type)
    type Warning = String
    type FreshNameGenerator = Int

    runCompiler :: Compiler a -> IO a
    runCompiler act = do
    	(res, warnings) <- evalStateT (runWriterT (runExceptT (evalStateT (runReaderT act []) ()))) 0
    	traverse putStrLn warnings
    	case res of
    		Left err -> die (show err)
    		Right a -> pure a

Now for some actual good stuff. We can still use `mtl`-style polymorphism
freely when there is no ambiguity about which part of the stack should provide
a particular effect. For example, `Compiler` has only one `WriterT`, so there's
no problem knowing which `tell` to use:

    warn :: MonadWriter [Warning] m => Warning -> m ()
    warn w = tell [w]

Similarly, if we want to use the top-level state, we can still use `mtl` if we
want:

    unify :: ( MonadState UnificationState m
             , MonadError TypeError m
             ) => Type -> Type -> m ()
    unify t1 t2 = do
    	us <- get
    	if t1 == t2
    	then put ()
    	else throwError (t1, t2)

Alternately, we can write the same type signature using `effect-stack` types
explicitly saying that we want these effects to be provided by the top-most
transformer that can provide them:

    unify' :: ( MonadStateDepth 0 m UnificationState
              , MonadErrorDepth 0 m TypeError
              ) => Type -> Type -> m ()
    unify' t1 t2 = do
    	us <- get
    	if t1 == t2
    	then put ()
    	else throwError (t1, t2)

Each kind of effect's stack is 0-indexed, so the outermost layer is layer 0. If
we want to access effects not provided by the top-most transformer, then we
must use `effect-stack` types (like `MonadStateDepth`) and methods (like
`depthState`).

    freshName :: MonadStateDepth 1 m FreshNameGenerator => m Name
    freshName = depthState @1 $ do
    	modify (1+)
    	gets show

We can also mix and match, both at the type level (using `mtl`, `base`, and
`effect-stack` classes), and within `do` blocks at the computation level (using
`mtl`-style transformer polymorphic methods, `base`-style polymorphic lifting
methods, and `effect-stack`-style polymorphic lifting methods).

    debug :: ( MonadStateDepth 1 m FreshNameGenerator
             , MonadReader ScopingInformation m
             , MonadIO m
             ) => m ()
    debug = do
    	n <- depthState @1 get
    	env <- ask
    	liftIO (print (n, env))

Here's a `main` that exists just to show that all the pieces can now be
specialized to the `Compiler` type as we wanted:

    main :: IO ()
    main = runCompiler $ do
    	unify () ()
    	warn "PHP is still more popular than Haskell."
    	v <- freshName
    	local (v:) $ do
    		unify' () ()
    		debug

Running it exits successfully after printing

    (1,["1"])
    PHP is still more popular than Haskell.

## ...use a new transformer with existing kinds of effects?

You can write new instances for the existing effect stack classes for your
transformer. You must first decide whether the transformer you are writing an
instance for provides the effect the class provides a stack for or not. For
example, for the `StateStack` class, does your transformer provide access to
some kind of stateful effect?

If it does, write an instance in which the `Pop<Effect>` family immediately
returns the monad being transformed, and `lift<Effect>` is just `lift`. For
example, because the `AccumT` family of transformers provides the
`Accum`ulation effect, the library provides this instance:

    instance (Monad m, Monoid w) => AccumStack (AccumT w m) where
    	type PopAccum (AccumT w m) = m
    	liftAccum = lift

The `(Monad m, Monoid w)` constraints are needed to satisfy the `Monad`
superclass of `AccumStack`; all the `<Effect>Stack` classes have this
superclass for user convenience.

If your transformer does not provide the effect, you should write an instance
that passes everything down one layer: `Pop<Effect>` should recurse on the
transformed monad, and `lift<Effect>` should be `lift . lift<Effect>`. For
example, since `MaybeT` does not provide `Accum`ulation effects, the library
provides this instance:

    instance AccumStack m => AccumStack (MaybeT m) where
    	type PopAccum (MaybeT m) = PopAccum m
    	liftAccum = lift . liftAccum

## ...create an effect stack for a new kind of effect?

You will want to create a new class for the effect that provides the low-level
tools for popping one layer of the stack at a time at the type level and
lifting one layer at a time at the computation level. Once you have done that,
there are some tools in `Control.Monad.Stack.Internal` that will be helpful for
creating the high-level interface that accepts type-level numbers and iterates
the low-level operations.

The classes are all quite similar to each other; you should be able to follow
the exact same pattern for each new effect. The class itself should look like
this:

    class Monad m => <Effect>Stack m where
    	type Pop<Effect> m :: * -> *
    	lift<Effect> :: Pop<Effect> m a -> m a

At this point you will need to choose a type-level token that can uniquely
identify this kind of effect. The type families that need this token as an
argument are poly-kinded and will accept a token of any kind here. For most of
the effects in this library, the token was chosen to be one of the transformers
that is typically used to provide the effect. You may also simply invent a new
type without exporting it if you are paranoid about collisions. Once you have
chosen a token, make a mapping from the token to the family created above:

    type instance Pop <Token> m = Pop<Effect> m

This token may now be passed to `IteratePop`, `StackConstraints`, and `depth`
to provide the high-level interface:

    type <Effect>Depth n m = IteratePop n <Token> m
    type <Effect>Constraints n m = (KnownNat n, StackConstraints n <Token> <Effect>Stack m)

    depth<Effect> :: forall n m a. <Effect>Constraints n m a => <Effect>Depth n m a -> m a
    depth<Effect> = depth @n @<Token> @<Effect>Stack lift<Effect>

If there is a class associated with the effect, you may also want to offer a
suitably stackified version of that class:

    type Monad<Effect>Depth n m <args> =
    	( <Effect>Constraints n m
    	, Monad<Effect> <args> (<Effect>Depth n m)
    	)

You will probably also want to write a bunch of instances for existing
transformers. See the section "How do I...", subsection "...use a new
transformer with existing kinds of effects?" for more information on doing
this.

# Why not...

## ...type-based resolution?

One alternate method of selecting which of many copies of an effect to use from
a stack would be to look at the type being used for that effect. For example,
continuing the `Compiler` example form the "Why?" section, one might imagine
that one could write a class which used type inference to decide whether the
current stateful actions were mucking about with `FreshNameGenerator`s or
`UnificationState`s, and use that information to decide whether to `lift` once
or four times.

This approach has two main drawbacks:

1. It turns out that type inference fails to differentiate between the two
   situations surprisingly often. This puts an unusually high type-annotation
   burden on users. (Indeed, this is the standard justification for the
   functional dependency included in all `mtl` typeclasses.)
2. It still leaves you open to the problem of mixing effects which just happen,
   by coincidence, to need access to the same type in the effect. For example,
   suppose your compiler is tracking how many tab characters it has seen so
   that it can issue an appropriate warning. Even so, some local module might
   want to slap a transformer on top to add an effect for tracking the arity of
   the function currently being compiled. These both happen to be `Int`s, and
   so once again we have a disambiguation problem.

## ...type-level tags?

One could imagine adding a tag to each transformer, and using the tags to
differentiate which effect is wanted. For example, with suitably modified
transformers, one might write:

    data Tag = Unification | Fresh | Other
    type Compiler = ReaderT Other ScopingInformation
                  ( StateT Unification UnificationState
                  ( ExceptT Other TypeError
                  ( WriterT Other [Warning]
                  ( StateT Fresh FreshNameGenerator
                  ( IO
                  )))))

Then, instead of using type-level numbers to indicate the depth in a stack, one
would use the tag to indicate which part of the stack was meant; so one might
imagine writing something like:

    unify :: MonadState Unification UnificationState m => Type -> Type -> m ()
    unify t1 t2 = get @Unification >>= \us -> ...

    freshName :: MonadState Fresh FreshNameGenerator m => m Name
    freshName = modify @Fresh (...)

Convenient shorthands could be provided by the appropriate libraries for
selecting, say, the `()` tag by default when the stack of interest was
unambiguous about which layer should provide a given effect.

Unlike choosing by effect or choosing by effect+type, one need not worry about
collisions with tags; modules which want to transform an existing monad could
ensure they use fresh tags by just making a new data kind.

This approach has a lot going for it, and I'd love to see a competing library
attempt this. The main drawback is that it is all-or-nothing, in that the
existing `transformers` transformers do not have these tags. By contrast,
`effect-stack` interoperates smoothly with existing `transformers` stacks. This
means that

1. Existing projects can adopt this library without a big migration.
2. New projects can use just `transformers`+`mtl`, which are syntactically and
   conceptually very light, right up to the moment that they need something
   more complicated.
