# 2019 winter Haskell school in Sydney Australia



## Class timetables and planning

We are based on [CIS194](https://www.seas.upenn.edu/~cis194/spring13/lectures.html) which is a 12 week course and we will extend it with advanced contents.

Each class has average length of 2 hours. Each week, we will pick a time and place and announce it.



## Materials that we used for this class



Currently we are using [Yorgey's cis194 course](https://www.seas.upenn.edu/~cis194/spring13/lectures.html).

This course is widely recommended by Haskell learners.



## Homework and after class discussions



After each week's class, participants need to complete assignments from the cis194 course and submit to this repo as a [Pull Request](https://github.com/fp-works/2019-winter-Haskell-school/pulls).


You can skip the assignments, but remember:

> If you don't do your homework and expect to be a Haskell programmer in the end of this course just by coming to/reading the lecture, you are kidding yourself. &mdash;Shine Li



You can also raise any questions or issues when you are learning Haskell and raise it to [here](https://github.com/fp-works/2019-winter-Haskell-school/issues).



You can submit your assignment to this path `cis194/week1/Yourname`, replace Yourname with your own.


*If you don't do your homework and expect to be a Haskell programmer in the end of this course just by coming to/reading the lecture, you are kidding yourself.*


## Other resources
### Setup local development environment
- [install stack](https://docs.haskellstack.org/en/stable/README/)
- `stack new project name`
- Then you have the following choices:
  - Use GHCi. This is invoked via `stack ghci` under your project root.
    - Every time you change your code, you can use `:r` to reload your ghci session with latest code. It will show error if your code does not compile.
  - Use GHCid
    - install GHCid by running `stack install ghcid` under your project
    - run `ghcid --command "stack ghci --ghci-options=-fobject-code"`
    - The above command will lunach a `stack ghci` session with type checking and automatically re-check your code when it changes. It will print out errors when you code does not compile.
  - IDE extension
    - VSCode
      - `haskell-ghcid` It runs GHCid and put the feedback in the output window as well as inlining them
      - `Haskell Syntax Highlighting` to highlight Haskell syntax
      - There are other extensions like `Haskell Language Server`, `Haskero` which required much more complex set up.
    - vim/neovim
      - TBD
    - Emacs
      - TBD
    - atom
      - `atom-haskell` This is a plugin that installs a list of other plugins to support Haskell development

### Knowledge points
- There are valuable take away knowledge hiding in the comments of homework PRs, they are collected under https://github.com/fp-works/2019-winter-Haskell-school/wiki/Best-Practises-Tips for easy discovery.


## YouTube channel [Instant FP](https://www.youtube.com/channel/UC9Hfs-_0PtqNT9Az-Jf1ptQ)

We will upload Functional programming videos inregually into this Channel. You can subscribe to it.
