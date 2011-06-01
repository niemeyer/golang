// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package sync

import "runtime"

// Cond implements a condition variable, a rendezvous point
// for goroutines waiting for or announcing the occurrence
// of an event.
//
// Each Cond has an associated Locker L (often a *Mutex or *RWMutex),
// which must be held when changing the condition and
// when calling the Wait method.
type Cond struct {
	L          Locker  // held while observing or changing the condition
	m          Mutex   // held to avoid internal races
	oldwaiters int     // number of goroutines blocked on Wait for oldsema
	newwaiters int     // number of goroutines blocked on Wait for newsema
	oldsema    *uint32 // semaphore for old generation (wakes up first)
	newsema    *uint32 // semaphore for new generation (wakes up later)
}

// NewCond returns a new Cond with Locker l.
func NewCond(l Locker) *Cond {
	return &Cond{L: l}
}

// Wait atomically unlocks c.L and suspends execution
// of the calling goroutine.  After later resuming execution,
// Wait locks c.L before returning.
//
// Because L is not locked when Wait first resumes, the caller
// typically cannot assume that the condition is true when
// Wait returns.  Instead, the caller should Wait in a loop:
//
//    c.L.Lock()
//    for !condition() {
//        c.Wait()
//    }
//    ... make use of condition ...
//    c.L.Unlock()
//
func (c *Cond) Wait() {
	c.m.Lock()
	if c.newsema == nil {
		c.newsema = new(uint32)
	}
	s := c.newsema
	c.newwaiters++
	c.m.Unlock()
	c.L.Unlock()
	runtime.Semacquire(s)
	c.L.Lock()
}

// Signal wakes one goroutine waiting on c, if there is any.
//
// It is allowed but not required for the caller to hold c.L
// during the call.
func (c *Cond) Signal() {
	c.m.Lock()
	if c.oldwaiters == 0 && c.newwaiters > 0 {
		// Swap generations.
		c.oldwaiters = c.newwaiters
		c.oldsema = c.newsema
		c.newwaiters = 0
		c.newsema = nil
	}
	if c.oldwaiters > 0 {
		c.oldwaiters--
		runtime.Semrelease(c.oldsema)
	}
	c.m.Unlock()
}

// Broadcast wakes all goroutines waiting on c.
//
// It is allowed but not required for the caller to hold c.L
// during the call.
func (c *Cond) Broadcast() {
	c.m.Lock()
	if c.oldwaiters > 0 || c.newwaiters > 0 {
		for i := 0; i < c.oldwaiters; i++ {
			runtime.Semrelease(c.oldsema)
		}
		for i := 0; i < c.newwaiters; i++ {
			runtime.Semrelease(c.newsema)
		}
		c.oldwaiters = 0
		c.newwaiters = 0
		c.newsema = nil
	}
	c.m.Unlock()
}
