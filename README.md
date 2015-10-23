# Living Network Demo

This is a demonstration of a living network as an alternative to the World Wide Web.  It models a minimal operating system that initializes a set of primitive capabilities, then negotiates with remote agents to bootstrap an interactive system.  The idea is partly inspired by an [interview](http://www.drdobbs.com/architecture-and-design/interview-with-alan-kay/240003442?pgno=2) with Alan Kay.

This demo is not even close to being fit for real world use.  At the very least, besides extending beyond a local simulation, the system must protect itself from malicious code coming from remote agents.  Basic safety could be achieved with resource consumption quotas and an execution model that does not expose ambient authority.
