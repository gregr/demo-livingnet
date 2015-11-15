# Living Network Demo

This is a demonstration of a living network as an alternative to the World Wide Web.  It models a minimal operating system that initializes a set of primitive capabilities, then negotiates with remote agents to bootstrap an interactive system.  The idea is partly inspired by an [interview](http://www.drdobbs.com/architecture-and-design/interview-with-alan-kay/240003442?pgno=2) with Alan Kay.

This demo is not even close to being fit for real world use.  At the very least, besides extending beyond a local simulation, the system must protect itself from malicious code coming from remote agents.  Basic safety could be achieved with resource consumption quotas and an execution model that does not expose ambient authority.

Aside from safety concerns, the current system has not realized the potential for programmability by the user.  For a complete experience, the programming language implementation this system sits on must support highly interactive evaluation of negotiated programs.  It must be possible to visualize, pause, step through (manually or at a regular interval), modify, copy, or even pull components out of a program for use in other programs.  To get a better idea of what is possible, try out [Squeak Etoys](http://www.squeakland.org).

## Getting Started

1. Install [Racket 6 or greater](http://download.racket-lang.org/).
2. Install gregr-misc: `raco pkg install git://github.com/gregr/racket-misc`
3. Clone this repo and `cd` to it.
4. Start service and user machines.
   - To run with a clean slate (creating new machines), run:`./clean-start`
       - When asked if you'd like to install a server kernel, enter: `n`
   - To restart existing machines while preserving state, run: `./start`
5. Evaluate racket expressions within the REPL.

## Introduction

Upon booting, you will be given a racket REPL with access to machine capabilities and a suggestion for retrieving the MOTD.  Aside from basic "hardware" access, you should also see `get` and `negotiate` in the capabilities list.  These two operations form the heart of this minimal design.

* `get` is used to retrieve data, uninterpreted, from a remote source.  You may think of this as similar to `GET` in the WWW design, but without the weight and assumptions of HTTP.

* `negotiate` is used to retrieve and evaluate a program from a remote source.

And that's all you need to get going.  Consider the WWW, which specifies, up front and in great detail, the form and communication of content you may publish and consume, and the manner in which you may navigate and display that content.  Under the WWW assumptions, such specification is necessary for remote agents to properly interoperate.  In contrast, this design supports just-in-time interoperation via `negotiate`: because a remote agent can directly provide you with a program, it can teach your system how to interoperate with it.  And it can use whatever "protocol" it deems fit for the purpose.  All your system needs is an interpreter.

If you are even vaguely familiar with issues of web security, `negotiate` will sound dangerous.  Indeed, given a typical programming language running on a typical operating system, such an operation would be dangerous if left unchecked.  There are some standard approaches to mitigating this danger, depending on the particular technology:

* Flash or Java applets:
  - Danger is mitigated by running within a sandbox.  While sandboxed execution prevents programs from harming you, it also limits what you can do with them.

* Javascript in the browser:
  - Danger is mitigated by not providing much more than the ability to script the browser.  The limitations are similar to sandboxed execution.

* Downloading arbitrary, native desktop software from the internet to install and run locally:
  - Danger is mitigated by asking if you're really really sure you want to install/run something from company X.  You're not limited, but neither is the program.  Who knows what it will do to you?

In contrast to these approaches, this design illustrates how to mitigate danger through [capability-based security](https://en.wikipedia.org/wiki/Capability-based_security).  `negotiate` retrieves data from a remote source, interpreting it as a procedure expecting a single context argument.  This context represents all the power you are willing to hand over to it to do its work.  Typical programming languages expose ample amounts of ambient authority, allowing such a procedure to sabotage you.  But in a capability-secure programming language, this procedure will not have access to any dangerous capabilities that you do not explicitly hand over.  The remote program can be a first-class citizen, safe to run side-by-side with the rest of your program.

Caveat: while this demo points in the right direction, it is not itself secure as it does not make use of a capability-secure programming language.

## Summary

So what exactly is the new technology on display here?  There isn't really any.  The main contribution is to demonstrate an alternative arrangement of existing ideas.  This arrangement could be summed up as:

1. Distribute native desktop software at least as transparently as today's web content.
2. Run and cooperate safely with this software in a capability-secure environment.

The effect of this simple re-organization should not be underestimated.  The danger currently involved in installing and running native software from the internet creates friction, encouraging it to be bundled as large, monolithic applications whose components can't easily be reused.  By eliminating the danger and minimizing friction, it becomes affordable to distribute native software as smaller, more flexible modules that can be composed in unanticipated ways.  This may sound like today's installation and use of software libraries by programmers.  The difference is in granularity and transparency: safe and convenient even for the smallest components, even for the average user of a computer.

## Examples

In case you haven't found them on your own, here are the remote systems you may interact with:

* `(negotiate "motd.demo" '())`
    - This is a simple message-of-the-day server.

* `(negotiate "hypodoc.demo" '())`
    - This is an extremely-underpowered, mostly-untested collaborative "cloud-based" text editor.  It's ugly and probably full of bugs.

Although it's inconvenient at the moment, you may of course also `(get "remote-of-your-choice.demo" '())` to retrieve the remote program without actually executing it, so that you can modify or make other use of it from the REPL.

## TODO

- more sophisticated authority management
    - capability guardedness spectrum
    - logging, revocation, time-boxing, throttling, permission appeals

- tabbed multitasking terminal UI with interaction control panel for each process
    - multiplexer creates pipe-console for each new process
    - tabbed viewing of virtual console + control panel for each
        - control panel shows cap requests and includes ability to revoke caps at any time
    - UI considerations:
        - ability to pause/terminate child processes at any time
        - up-front requests necessary for dangerous capabilities when starting process
            - late request for a dangerous capability is auto-denied
                - process should be paused (to allow manual override) or killed outright
            - options
                - grant
                    - possibly also requesting logging, subsequent permission prompting, etc.
                - do not grant
                    - optionally may allow process to start anyway; because you're curious
            - options may also be conveniently set beforehand per-capability
                - globally and/or on a per-site basis
        - requests for user input to a capability
            - follow the usual rules, but if granted, provide an appropriate input dialog
            - examples
                - user asked to enter an account password into a pop-up text box
                - user asked to choose a file from a dialog box

- contexts
    - everything is optional and could be provided dynamically by asking the user for permission
        - some requests may be nonstandard and require intelligent agent to view the requesting code to figure out what it needs to proceed
        - request could include constraints such as full spec or just example inputs, outputs, effects
            - would enable more automation

- communicating with aliens
    - establishing conventions, deciphering foreign protocols, rosetta stones

notes on accessibility and privacy
