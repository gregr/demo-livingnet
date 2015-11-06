# Living Network Demo

This is a demonstration of a living network as an alternative to the World Wide Web.  It models a minimal operating system that initializes a set of primitive capabilities, then negotiates with remote agents to bootstrap an interactive system.  The idea is partly inspired by an [interview](http://www.drdobbs.com/architecture-and-design/interview-with-alan-kay/240003442?pgno=2) with Alan Kay.

This demo is not even close to being fit for real world use.  At the very least, besides extending beyond a local simulation, the system must protect itself from malicious code coming from remote agents.  Basic safety could be achieved with resource consumption quotas and an execution model that does not expose ambient authority.

Aside from safety concerns, the current system has not realized the potential for programmability by the user.  For a complete experience, the programming language implementation this system sits on must support highly interactive evaluation of negotiated programs.  It must be possible to visualize, pause, step through (manually or at a regular interval), modify, copy, or even pull components out of a program for use in other programs.  To get a better idea of what is possible, try out [Squeak Etoys](http://www.squeakland.org).

## TODO

more sophisticated authority management
  capability guardedness spectrum
  logging, revocation, time-boxing, throttling, permission appeals

tabbed multitasking terminal UI with interaction control panel for each process
  multiplexer creates pipe-console for each new process
  tabbed viewing of virtual console + control panel for each
    control panel shows cap requests and includes ability to revoke caps at any time
  UI considerations:
    ability to pause/terminate child processes at any time
    up-front requests necessary for dangerous capabilities when starting process
      late request for a dangerous capability is auto-denied
        process should be paused (to allow manual override) or killed outright
      options
        grant
          possibly also requesting logging, subsequent permission prompting, etc.
        do not grant
          optionally may allow process to start anyway; because you're curious
      options may also be conveniently set beforehand per-capability
        globally and/or on a per-site basis
    requests for user input to a capability
      follow the usual rules, but if granted, provide an appropriate input dialog
      examples
        user asked to enter an account password into a pop-up text box
        user asked to choose a file from a dialog box

contexts
  everything is optional and could be provided dynamically by asking the user for permission
    some requests may be nonstandard and require intelligent agent to view the requesting code to figure out what it needs to proceed
    request could include constraints such as full spec or just example inputs, outputs, effects
      would enable more automation

communicating with aliens
  establishing conventions, deciphering foreign protocols, rosetta stones

notes on accessibility and privacy
