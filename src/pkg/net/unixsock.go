// Copyright 2009 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Unix domain sockets

package net

import (
	"os"
)

// UnixAddr represents the address of a Unix domain socket end point.
type UnixAddr struct {
	Name string
	Net  string
}

// Network returns the address's network name, "unix" or "unixgram".
func (a *UnixAddr) Network() string {
	return a.Net
}

func (a *UnixAddr) String() string {
	if a == nil {
		return "<nil>"
	}
	return a.Name
}

func (a *UnixAddr) toAddr() Addr {
	if a == nil { // nil *UnixAddr
		return nil // nil interface
	}
	return a
}

// ResolveUnixAddr parses addr as a Unix domain socket address.
// The string net gives the network name, "unix", "unixgram" or
// "unixpacket".
func ResolveUnixAddr(net, addr string) (*UnixAddr, os.Error) {
	switch net {
	case "unix":
	case "unixpacket":
	case "unixgram":
	default:
		return nil, UnknownNetworkError(net)
	}
	return &UnixAddr{addr, net}, nil
}
