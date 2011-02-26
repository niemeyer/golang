// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
package main

import (
	"runtime"
	"testing"
)

func TestGoodOSArch(t *testing.T) {
	var thisOS, thisArch = runtime.GOOS, runtime.GOARCH
	var otherOS, otherArch string

	if thisOS == "darwin" {
		otherOS = "linux"
	} else {
		otherOS = "darwin"
	}
	if thisArch == "arm" {
		otherArch = "amd64"
	} else {
		otherArch = "arm"
	}

	var tests = []struct {
		name string
		res  bool
	}{
		{"file.go", true},
		{"file.c", true},
		{"file_foo.go", true},
		{"file_" + thisArch + ".go", true},
		{"file_" + otherArch + ".go", false},
		{"file_" + thisOS + ".go", true},
		{"file_" + otherOS + ".go", false},
		{"file_" + thisOS + "_" + thisArch + ".go", true},
		{"file_" + otherOS + "_" + thisArch + ".go", false},
		{"file_" + thisOS + "_" + otherArch + ".go", false},
		{"file_" + otherOS + "_" + otherArch + ".go", false},
		{"file_foo_" + thisArch + ".go", true},
		{"file_foo_" + otherArch + ".go", false},
		{"file_" + thisOS + ".c", true},
		{"file_" + otherOS + ".c", false},
	}

	for _, test := range tests {
		if goodOSArch(test.name) != test.res {
			t.Fatalf("goodOSArch(%q) != %s", test.name, test.res)
		}
	}
}
