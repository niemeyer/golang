// errchk $G $D/$F.go

// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// issue 1993.
// error used to have last line number in file

package main

func bla1() bool {
	return false
}

func bla5() bool {
	_ = 1
	false  // ERROR "false not used"
	_ = 2
}

func main() {
	x := bla1()
	_ = x
}
