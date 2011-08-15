#!/usr/bin/env bash
# Copyright 2010 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

GOROOT=$(cd `dirname $0`/..; pwd)

# If hg doesn't work, try VERSION file or fail otherwise
if [ ! -d "$GOROOT/.hg" ] || ! hg version > /dev/null 2>&1; then
	if [ -f "$GOROOT/VERSION" ]; then
		cat $GOROOT/VERSION
		exit 0
	else
		echo 'Unable to report version: hg and VERSION file missing' 1>&2
		echo 'Generate VERSION with `src/version.bash -save` while hg is usable' 1>&2
		exit 2
	fi
fi

# Get numerical revision
VERSION=$(hg identify -n 2>/dev/null)
if [ $? != 0 ]; then
	OLD=$(hg identify | sed 1q)
	VERSION=$(echo $OLD | awk '{print $1}')
fi

# Get branch type
BRANCH=release
if [ "$(hg identify -b 2>/dev/null)" == "default" ]; then
	BRANCH=weekly
fi

# Find most recent known release or weekly tag.
TAG=$(hg tags |
	grep $BRANCH |
	sed 's/:.*//' |
	sort -rn -k2 |
	awk -v ver=$VERSION '$2 <= ver && $1~/^(release|weekly)\./ {print $1}' |
	sed -n 1p)

if [ "$TAG" != "" ]; then
	VERSION="$TAG $VERSION"
fi

if [ "$1" = "-save" ]; then
	echo $VERSION > $GOROOT/VERSION
	echo "Saved '$VERSION' to $GOROOT/VERSION" 1>&2
else
	echo $VERSION
fi
