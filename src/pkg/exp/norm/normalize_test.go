// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package norm

import (
	"strings"
	"testing"
)

type PositionTest struct {
	input  string
	pos    int
	buffer string // expected contents of reorderBuffer, if applicable
}

type positionFunc func(rb *reorderBuffer, s string) int

func runPosTests(t *testing.T, name string, f Form, fn positionFunc, tests []PositionTest) {
	rb := reorderBuffer{f: *formTable[f]}
	for i, test := range tests {
		rb.reset()
		pos := fn(&rb, test.input)
		if pos != test.pos {
			t.Errorf("%s:%d: position is %d; want %d", name, i, pos, test.pos)
		}
		runes := []int(test.buffer)
		if rb.nrune != len(runes) {
			t.Errorf("%s:%d: reorder buffer lenght is %d; want %d", name, i, rb.nrune, len(runes))
			continue
		}
		for j, want := range runes {
			found := int(rb.runeAt(j))
			if found != want {
				t.Errorf("%s:%d: rune at %d is %U; want %U", name, i, j, found, want)
			}
		}
	}
}

var decomposeSegmentTests = []PositionTest{
	// illegal runes
	{"\xC0", 0, ""},
	{"\u00E0\x80", 2, "\u0061\u0300"},
	// starter
	{"a", 1, "a"},
	{"ab", 1, "a"},
	// starter + composing
	{"a\u0300", 3, "a\u0300"},
	{"a\u0300b", 3, "a\u0300"},
	// with decomposition
	{"\u00C0", 2, "A\u0300"},
	{"\u00C0b", 2, "A\u0300"},
	// long
	{strings.Repeat("\u0300", 32), 64, strings.Repeat("\u0300", 32)},
	// overflow
	{strings.Repeat("\u0300", 33), 64, strings.Repeat("\u0300", 32)},
	// ends with incomplete UTF-8 encoding
	{"\xCC", 0, ""},
	{"\u0300\xCC", 2, "\u0300"},
}

func decomposeSegmentF(rb *reorderBuffer, s string) int {
	return decomposeSegment(rb, []byte(s))
}

func TestDecomposeSegment(t *testing.T) {
	runPosTests(t, "TestDecomposeSegment", NFC, decomposeSegmentF, decomposeSegmentTests)
}

var firstBoundaryTests = []PositionTest{
	// no boundary
	{"", -1, ""},
	{"\u0300", -1, ""},
	{"\x80\x80", -1, ""},
	// illegal runes
	{"\xff", 0, ""},
	{"\u0300\xff", 2, ""},
	{"\u0300\xc0\x80\x80", 2, ""},
	// boundaries
	{"a", 0, ""},
	{"\u0300a", 2, ""},
	// Hangul
	{"\u1103\u1161", 0, ""},
	{"\u110B\u1173\u11B7", 0, ""},
	{"\u1161\u110B\u1173\u11B7", 3, ""},
	{"\u1173\u11B7\u1103\u1161", 6, ""},
}

func firstBoundary(rb *reorderBuffer, s string) int {
	return rb.f.form.FirstBoundary([]byte(s))
}

func TestFirstBoundary(t *testing.T) {
	runPosTests(t, "TestFirstBoundary", NFC, firstBoundary, firstBoundaryTests)
}

var decomposeToLastTests = []PositionTest{
	// ends with inert character
	{"Hello!", 6, ""},
	{"\u0632", 2, ""},
	{"a\u0301\u0635", 5, ""},
	// ends with non-inert starter
	{"a", 0, "a"},
	{"a\u0301a", 3, "a"},
	{"a\u0301\u03B9", 3, "\u03B9"},
	// illegal runes
	{"\xFF", 1, ""},
	{"aa\xFF", 3, ""},
	{"\xC0\x80\x80", 3, ""},
	{"\xCC\x80\x80", 3, ""},
	// ends with incomplete UTF-8 encoding
	{"a\xCC", 2, ""},
	// ends with combining characters
	{"\u0300\u0301", 0, "\u0300\u0301"},
	{"a\u0300\u0301", 0, "a\u0300\u0301"},
	{"a\u0301\u0308", 0, "a\u0301\u0308"},
	{"a\u0308\u0301", 0, "a\u0308\u0301"},
	{"aaaa\u0300\u0301", 3, "a\u0300\u0301"},
	{"\u0300a\u0300\u0301", 2, "a\u0300\u0301"},
	{"\u00C0", 0, "A\u0300"},
	{"a\u00C0", 1, "A\u0300"},
	// decomposing
	{"a\u0300\uFDC0", 3, "\u0645\u062C\u064A"},
	{"\uFDC0" + strings.Repeat("\u0300", 28), 0, "\u0645\u062C\u064A" + strings.Repeat("\u0300", 28)},
	// Hangul
	{"a\u1103", 1, "\u1103"},
	{"a\u110B", 1, "\u110B"},
	{"a\u110B\u1173", 1, "\u110B\u1173"},
	// See comment in composition.go:compBoundaryAfter.
	{"a\u110B\u1173\u11B7", 1, "\u110B\u1173\u11B7"},
	{"a\uC73C", 1, "\u110B\u1173"},
	{"다음", 3, "\u110B\u1173\u11B7"},
	{"다", 0, "\u1103\u1161"},
	{"\u1103\u1161\u110B\u1173\u11B7", 6, "\u110B\u1173\u11B7"},
	{"\u110B\u1173\u11B7\u1103\u1161", 9, "\u1103\u1161"},
	{"다음음", 6, "\u110B\u1173\u11B7"},
	{"음다다", 6, "\u1103\u1161"},
	// buffer overflow
	{"a" + strings.Repeat("\u0300", 35), 9, strings.Repeat("\u0300", 31)},
	{"\uFDFA" + strings.Repeat("\u0300", 14), 3, strings.Repeat("\u0300", 14)},
	// weird UTF-8
	{"a\u0300\u11B7", 0, "a\u0300\u11B7"},
}

func decomposeToLast(rb *reorderBuffer, s string) int {
	buf := decomposeToLastBoundary(rb, []byte(s))
	return len(buf)
}

func TestDecomposeToLastBoundary(t *testing.T) {
	runPosTests(t, "TestDecomposeToLastBoundary", NFKC, decomposeToLast, decomposeToLastTests)
}

var lastBoundaryTests = []PositionTest{
	// ends with inert character
	{"Hello!", 6, ""},
	{"\u0632", 2, ""},
	// ends with non-inert starter
	{"a", 0, ""},
	// illegal runes
	{"\xff", 1, ""},
	{"aa\xff", 3, ""},
	{"a\xff\u0300", 1, ""},
	{"\xc0\x80\x80", 3, ""},
	{"\xc0\x80\x80\u0300", 3, ""},
	{"\xc0", 1, ""},
	// ends with combining characters
	{"a\u0300\u0301", 0, ""},
	{"aaaa\u0300\u0301", 3, ""},
	{"\u0300a\u0300\u0301", 2, ""},
	{"\u00C0", 0, ""},
	{"a\u00C0", 1, ""},
	// no boundary
	{"", -1, ""},
	{"\u0300\u0301", -1, ""},
	{"\u0300", -1, ""},
	{"\x80\x80", -1, ""},
	{"\x80\x80\u0301", -1, ""},
	// Hangul
	{"다음", 3, ""},
	{"다", 0, ""},
	{"\u1103\u1161\u110B\u1173\u11B7", 6, ""},
	{"\u110B\u1173\u11B7\u1103\u1161", 9, ""},
	// ends with incomplete UTF-8 encoding
	{"\xCC", 1, ""},
}

func lastBoundary(rb *reorderBuffer, s string) int {
	return rb.f.form.LastBoundary([]byte(s))
}

func TestLastBoundary(t *testing.T) {
	runPosTests(t, "TestLastBoundary", NFC, lastBoundary, lastBoundaryTests)
}

var quickSpanTests = []PositionTest{
	{"", 0, ""},
	// starters
	{"a", 1, ""},
	{"abc", 3, ""},
	{"\u043Eb", 3, ""},
	// incomplete last rune.
	{"\xCC", 1, ""},
	{"a\xCC", 2, ""},
	// incorrectly ordered combining characters
	{"\u0300\u0316", 0, ""},
	{"\u0300\u0316cd", 0, ""},
}

var quickSpanNFDTests = []PositionTest{
	// needs decomposing
	{"\u00C0", 0, ""},
	{"abc\u00C0", 3, ""},
	// correctly ordered combining characters
	{"\u0300", 2, ""},
	{"ab\u0300", 4, ""},
	{"ab\u0300cd", 6, ""},
	{"\u0300cd", 4, ""},
	{"\u0316\u0300", 4, ""},
	{"ab\u0316\u0300", 6, ""},
	{"ab\u0316\u0300cd", 8, ""},
	{"ab\u0316\u0300\u00C0", 6, ""},
	{"\u0316\u0300cd", 6, ""},
	{"\u043E\u0308b", 5, ""},
	// incorrectly ordered combining characters
	{"ab\u0300\u0316", 1, ""}, // TODO(mpvl): we could skip 'b' as well.
	{"ab\u0300\u0316cd", 1, ""},
	// Hangul
	{"같은", 0, ""},
}

var quickSpanNFCTests = []PositionTest{
	// okay composed
	{"\u00C0", 2, ""},
	{"abc\u00C0", 5, ""},
	// correctly ordered combining characters
	{"ab\u0300", 1, ""},
	{"ab\u0300cd", 1, ""},
	{"ab\u0316\u0300", 1, ""},
	{"ab\u0316\u0300cd", 1, ""},
	{"\u00C0\u035D", 4, ""},
	// we do not special case leading combining characters
	{"\u0300cd", 0, ""},
	{"\u0300", 0, ""},
	{"\u0316\u0300", 0, ""},
	{"\u0316\u0300cd", 0, ""},
	// incorrectly ordered combining characters
	{"ab\u0300\u0316", 1, ""},
	{"ab\u0300\u0316cd", 1, ""},
	// Hangul
	{"같은", 6, ""},
}

func doQuickSpan(rb *reorderBuffer, s string) int {
	return rb.f.form.QuickSpan([]byte(s))
}

func TestQuickSpan(t *testing.T) {
	runPosTests(t, "TestQuickSpanNFD1", NFD, doQuickSpan, quickSpanTests)
	runPosTests(t, "TestQuickSpanNFD2", NFD, doQuickSpan, quickSpanNFDTests)
	runPosTests(t, "TestQuickSpanNFC1", NFC, doQuickSpan, quickSpanTests)
	runPosTests(t, "TestQuickSpanNFC2", NFC, doQuickSpan, quickSpanNFCTests)
}

var isNormalTests = []PositionTest{
	{"", 1, ""},
	// illegal runes
	{"\xff", 1, ""},
	// starters
	{"a", 1, ""},
	{"abc", 1, ""},
	{"\u043Eb", 1, ""},
	// incorrectly ordered combining characters
	{"\u0300\u0316", 0, ""},
	{"ab\u0300\u0316", 0, ""},
	{"ab\u0300\u0316cd", 0, ""},
	{"\u0300\u0316cd", 0, ""},
}
var isNormalNFDTests = []PositionTest{
	// needs decomposing
	{"\u00C0", 0, ""},
	{"abc\u00C0", 0, ""},
	// correctly ordered combining characters
	{"\u0300", 1, ""},
	{"ab\u0300", 1, ""},
	{"ab\u0300cd", 1, ""},
	{"\u0300cd", 1, ""},
	{"\u0316\u0300", 1, ""},
	{"ab\u0316\u0300", 1, ""},
	{"ab\u0316\u0300cd", 1, ""},
	{"\u0316\u0300cd", 1, ""},
	{"\u043E\u0308b", 1, ""},
	// Hangul
	{"같은", 0, ""},
}
var isNormalNFCTests = []PositionTest{
	// okay composed
	{"\u00C0", 1, ""},
	{"abc\u00C0", 1, ""},
	// need reordering
	{"a\u0300", 0, ""},
	{"a\u0300cd", 0, ""},
	{"a\u0316\u0300", 0, ""},
	{"a\u0316\u0300cd", 0, ""},
	// correctly ordered combining characters
	{"ab\u0300", 1, ""},
	{"ab\u0300cd", 1, ""},
	{"ab\u0316\u0300", 1, ""},
	{"ab\u0316\u0300cd", 1, ""},
	{"\u00C0\u035D", 1, ""},
	{"\u0300", 1, ""},
	{"\u0316\u0300cd", 1, ""},
	// Hangul
	{"같은", 1, ""},
}

func isNormal(rb *reorderBuffer, s string) int {
	if rb.f.form.IsNormal([]byte(s)) {
		return 1
	}
	return 0
}

func TestIsNormal(t *testing.T) {
	runPosTests(t, "TestIsNormalNFD1", NFD, isNormal, isNormalTests)
	runPosTests(t, "TestIsNormalNFD2", NFD, isNormal, isNormalNFDTests)
	runPosTests(t, "TestIsNormalNFC1", NFC, isNormal, isNormalTests)
	runPosTests(t, "TestIsNormalNFC2", NFC, isNormal, isNormalNFCTests)
}

type AppendTest struct {
	left  string
	right string
	out   string
}

type appendFunc func(f Form, out []byte, s string) []byte

func runAppendTests(t *testing.T, name string, f Form, fn appendFunc, tests []AppendTest) {
	for i, test := range tests {
		out := []byte(test.left)
		out = fn(f, out, test.right)
		if string(out) != test.out {
			t.Errorf("%s:%d: result is %X; want %X", name, i, []int(string(out)), []int(test.out))
		}
	}
}

var appendTests = []AppendTest{
	// empty buffers
	{"", "", ""},
	{"a", "", "a"},
	{"", "a", "a"},
	{"", "\u0041\u0307\u0304", "\u01E0"},
	// segment split across buffers
	{"", "a\u0300b", "\u00E0b"},
	{"a", "\u0300b", "\u00E0b"},
	{"a", "\u0300a\u0300", "\u00E0\u00E0"},
	{"a", "\u0300a\u0300a\u0300", "\u00E0\u00E0\u00E0"},
	{"a", "\u0300aaa\u0300aaa\u0300", "\u00E0aa\u00E0aa\u00E0"},
	{"a\u0300", "\u0316", "\u00E0\u0316"},
	{"\u0041\u0307", "\u0304", "\u01E0"},
	// Hangul
	{"", "\u110B\u1173", "\uC73C"},
	{"", "\u1103\u1161", "\uB2E4"},
	{"", "\u110B\u1173\u11B7", "\uC74C"},
	{"", "\u320E", "\x28\uAC00\x29"},
	{"", "\x28\u1100\u1161\x29", "\x28\uAC00\x29"},
	{"\u1103", "\u1161", "\uB2E4"},
	{"\u110B", "\u1173\u11B7", "\uC74C"},
	{"\u110B\u1173", "\u11B7", "\uC74C"},
	{"\uC73C", "\u11B7", "\uC74C"},
	// UTF-8 encoding split across buffers
	{"a\xCC", "\x80", "\u00E0"},
	{"a\xCC", "\x80b", "\u00E0b"},
	{"a\xCC", "\x80a\u0300", "\u00E0\u00E0"},
	{"a\xCC", "\x80\x80", "\u00E0\x80"},
	{"a\xCC", "\x80\xCC", "\u00E0\xCC"},
	// ending in incomplete UTF-8 encoding
	{"", "\xCC", "\xCC"},
	{"a", "\xCC", "a\xCC"},
	{"a", "b\xCC", "ab\xCC"},
	// illegal runes
	{"", "\x80", "\x80"},
	{"", "\x80\x80\x80", "\x80\x80\x80"},
	{"", "\xCC\x80\x80\x80", "\xCC\x80\x80\x80"},
	{"", "a\x80", "a\x80"},
	{"", "a\x80\x80\x80", "a\x80\x80\x80"},
	{"", "a\x80\x80\x80\x80\x80\x80", "a\x80\x80\x80\x80\x80\x80"},
	{"a", "\x80\x80\x80", "a\x80\x80\x80"},
	// overflow
	{"", strings.Repeat("\x80", 33), strings.Repeat("\x80", 33)},
	{strings.Repeat("\x80", 33), "", strings.Repeat("\x80", 33)},
	{strings.Repeat("\x80", 33), strings.Repeat("\x80", 33), strings.Repeat("\x80", 66)},
	{"", strings.Repeat("\u0300", 33), strings.Repeat("\u0300", 33)},
	{strings.Repeat("\u0300", 33), "", strings.Repeat("\u0300", 33)},
	{strings.Repeat("\u0300", 33), strings.Repeat("\u0300", 33), strings.Repeat("\u0300", 66)},
	// weird UTF-8
	{"a\u0300\u11B7", "\u0300", "\u00E0\u11B7\u0300"},
	{"a\u0300\u11B7\u0300", "\u0300", "\u00E0\u11B7\u0300\u0300"},
}

func appendF(f Form, out []byte, s string) []byte {
	return f.Append(out, []byte(s)...)
}

func TestAppend(t *testing.T) {
	runAppendTests(t, "TestAppend", NFKC, appendF, appendTests)
}

func doFormBenchmark(b *testing.B, f Form, s string) {
	b.StopTimer()
	in := []byte(s)
	buf := make([]byte, 2*len(in))
	b.SetBytes(int64(len(s)))
	b.StartTimer()
	for i := 0; i < b.N; i++ {
		buf = f.Append(buf[0:0], in...)
		buf = buf[0:0]
	}
}

var ascii = strings.Repeat("There is nothing to change here! ", 500)

func BenchmarkNormalizeAsciiNFC(b *testing.B) {
	doFormBenchmark(b, NFC, ascii)
}
func BenchmarkNormalizeAsciiNFD(b *testing.B) {
	doFormBenchmark(b, NFD, ascii)
}
func BenchmarkNormalizeAsciiNFKC(b *testing.B) {
	doFormBenchmark(b, NFKC, ascii)
}
func BenchmarkNormalizeAsciiNFKD(b *testing.B) {
	doFormBenchmark(b, NFKD, ascii)
}

func doTextBenchmark(b *testing.B, s string) {
	b.StopTimer()
	in := make([]byte, len(s))
	for i := range s {
		in[i] = s[i]
	}
	// Using copy(in, s) makes many tests much slower!?
	b.SetBytes(int64(len(s)) * 4)
	var buf = make([]byte, 2*len(in))
	b.StartTimer()
	for i := 0; i < b.N; i++ {
		buf = NFC.Append(buf[0:0], in...)
		buf = NFD.Append(buf[0:0], in...)
		buf = NFKC.Append(buf[0:0], in...)
		buf = NFKD.Append(buf[0:0], in...)
	}
}

func BenchmarkCanonicalOrdering(b *testing.B) {
	doTextBenchmark(b, txt_canon)
}
func BenchmarkExtendedLatin(b *testing.B) {
	doTextBenchmark(b, txt_vn)
}
func BenchmarkMiscTwoByteUtf8(b *testing.B) {
	doTextBenchmark(b, twoByteUtf8)
}
func BenchmarkMiscThreeByteUtf8(b *testing.B) {
	doTextBenchmark(b, threeByteUtf8)
}
func BenchmarkHangul(b *testing.B) {
	doTextBenchmark(b, txt_kr)
}
func BenchmarkJapanese(b *testing.B) {
	doTextBenchmark(b, txt_jp)
}
func BenchmarkChinese(b *testing.B) {
	doTextBenchmark(b, txt_cn)
}

// Tests sampled from the Canonical ordering tests (Part 2) of
// http://unicode.org/Public/UNIDATA/NormalizationTest.txt
const txt_canon = `\u0061\u0315\u0300\u05AE\u0300\u0062 \u0061\u0300\u0315\u0300\u05AE\u0062
\u0061\u0302\u0315\u0300\u05AE\u0062 \u0061\u0307\u0315\u0300\u05AE\u0062
\u0061\u0315\u0300\u05AE\u030A\u0062 \u0061\u059A\u0316\u302A\u031C\u0062
\u0061\u032E\u059A\u0316\u302A\u0062 \u0061\u0338\u093C\u0334\u0062 
\u0061\u059A\u0316\u302A\u0339       \u0061\u0341\u0315\u0300\u05AE\u0062
\u0061\u0348\u059A\u0316\u302A\u0062 \u0061\u0361\u0345\u035D\u035C\u0062
\u0061\u0366\u0315\u0300\u05AE\u0062 \u0061\u0315\u0300\u05AE\u0486\u0062
\u0061\u05A4\u059A\u0316\u302A\u0062 \u0061\u0315\u0300\u05AE\u0613\u0062
\u0061\u0315\u0300\u05AE\u0615\u0062 \u0061\u0617\u0315\u0300\u05AE\u0062
\u0061\u0619\u0618\u064D\u064E\u0062 \u0061\u0315\u0300\u05AE\u0654\u0062
\u0061\u0315\u0300\u05AE\u06DC\u0062 \u0061\u0733\u0315\u0300\u05AE\u0062
\u0061\u0744\u059A\u0316\u302A\u0062 \u0061\u0315\u0300\u05AE\u0745\u0062
\u0061\u09CD\u05B0\u094D\u3099\u0062 \u0061\u0E38\u0E48\u0E38\u0C56\u0062
\u0061\u0EB8\u0E48\u0E38\u0E49\u0062 \u0061\u0F72\u0F71\u0EC8\u0F71\u0062
\u0061\u1039\u05B0\u094D\u3099\u0062 \u0061\u05B0\u094D\u3099\u1A60\u0062
\u0061\u3099\u093C\u0334\u1BE6\u0062 \u0061\u3099\u093C\u0334\u1C37\u0062
\u0061\u1CD9\u059A\u0316\u302A\u0062 \u0061\u2DED\u0315\u0300\u05AE\u0062
\u0061\u2DEF\u0315\u0300\u05AE\u0062 \u0061\u302D\u302E\u059A\u0316\u0062`

// Taken from http://creativecommons.org/licenses/by-sa/3.0/vn/
const txt_vn = `Với các điều kiện sau: Ghi nhận công của tác giả. 
Nếu bạn sử dụng, chuyển đổi, hoặc xây dựng dự án từ 
nội dung được chia sẻ này, bạn phải áp dụng giấy phép này hoặc 
một giấy phép khác có các điều khoản tương tự như giấy phép này
cho dự án của bạn. Hiểu rằng: Miễn — Bất kỳ các điều kiện nào
trên đây cũng có thể được miễn bỏ nếu bạn được sự cho phép của
người sở hữu bản quyền. Phạm vi công chúng — Khi tác phẩm hoặc
bất kỳ chương nào của tác phẩm đã trong vùng dành cho công
chúng theo quy định của pháp luật thì tình trạng của nó không 
bị ảnh hưởng bởi giấy phép trong bất kỳ trường hợp nào.`

// Taken from http://creativecommons.org/licenses/by-sa/1.0/deed.ru
const txt_ru = `При обязательном соблюдении следующих условий:
Attribution — Вы должны атрибутировать произведение (указывать
автора и источник) в порядке, предусмотренном автором или
лицензиаром (но только так, чтобы никоим образом не подразумевалось,
что они поддерживают вас или использование вами данного произведения).
Υπό τις ακόλουθες προϋποθέσεις:`

// Taken from http://creativecommons.org/licenses/by-sa/3.0/gr/
const txt_gr = `Αναφορά Δημιουργού — Θα πρέπει να κάνετε την αναφορά στο έργο με τον
τρόπο που έχει οριστεί από το δημιουργό ή το χορηγούντο την άδεια
(χωρίς όμως να εννοείται με οποιονδήποτε τρόπο ότι εγκρίνουν εσάς ή
τη χρήση του έργου από εσάς). Παρόμοια Διανομή — Εάν αλλοιώσετε,
τροποποιήσετε ή δημιουργήσετε περαιτέρω βασισμένοι στο έργο θα
μπορείτε να διανέμετε το έργο που θα προκύψει μόνο με την ίδια ή
παρόμοια άδεια.`

// Taken from http://creativecommons.org/licenses/by-sa/3.0/deed.ar
const txt_ar = `بموجب الشروط التالية نسب المصنف — يجب عليك أن
تنسب العمل بالطريقة التي تحددها المؤلف أو المرخص (ولكن ليس بأي حال من
الأحوال أن توحي وتقترح بتحول أو استخدامك للعمل).
المشاركة على قدم المساواة — إذا كنت يعدل ، والتغيير ، أو الاستفادة
من هذا العمل ، قد ينتج عن توزيع العمل إلا في ظل تشابه او تطابق فى واحد
لهذا الترخيص.`

// Taken from http://creativecommons.org/licenses/by-sa/1.0/il/
const txt_il = `בכפוף לתנאים הבאים: ייחוס — עליך לייחס את היצירה (לתת קרדיט) באופן
המצויין על-ידי היוצר או מעניק הרישיון (אך לא בשום אופן המרמז על כך
שהם תומכים בך או בשימוש שלך ביצירה). שיתוף זהה — אם תחליט/י לשנות,
לעבד או ליצור יצירה נגזרת בהסתמך על יצירה זו, תוכל/י להפיץ את יצירתך
החדשה רק תחת אותו הרישיון או רישיון דומה לרישיון זה.`

const twoByteUtf8 = txt_ru + txt_gr + txt_ar + txt_il

// Taken from http://creativecommons.org/licenses/by-sa/2.0/kr/
const txt_kr = `다음과 같은 조건을 따라야 합니다: 저작자표시
(Attribution) — 저작자나 이용허락자가 정한 방법으로 저작물의
원저작자를 표시하여야 합니다(그러나 원저작자가 이용자나 이용자의
이용을 보증하거나 추천한다는 의미로 표시해서는 안됩니다). 
동일조건변경허락 — 이 저작물을 이용하여 만든 이차적 저작물에는 본
라이선스와 동일한 라이선스를 적용해야 합니다.`

// Taken from http://creativecommons.org/licenses/by-sa/3.0/th/
const txt_th = `ภายใต้เงื่อนไข ดังต่อไปนี้ : แสดงที่มา — คุณต้องแสดงที่
มาของงานดังกล่าว ตามรูปแบบที่ผู้สร้างสรรค์หรือผู้อนุญาตกำหนด (แต่
ไม่ใช่ในลักษณะที่ว่า พวกเขาสนับสนุนคุณหรือสนับสนุนการที่
คุณนำงานไปใช้) อนุญาตแบบเดียวกัน — หากคุณดัดแปลง เปลี่ยนรูป หรื
อต่อเติมงานนี้ คุณต้องใช้สัญญาอนุญาตแบบเดียวกันหรือแบบที่เหมื
อนกับสัญญาอนุญาตที่ใช้กับงานนี้เท่านั้น`

const threeByteUtf8 = txt_th

// Taken from http://creativecommons.org/licenses/by-sa/2.0/jp/
const txt_jp = `あなたの従うべき条件は以下の通りです。
表示 — あなたは原著作者のクレジットを表示しなければなりません。
継承 — もしあなたがこの作品を改変、変形または加工した場合、
あなたはその結果生じた作品をこの作品と同一の許諾条件の下でのみ
頒布することができます。`

// http://creativecommons.org/licenses/by-sa/2.5/cn/
const txt_cn = `您可以自由： 复制、发行、展览、表演、放映、
广播或通过信息网络传播本作品 创作演绎作品
对本作品进行商业性使用 惟须遵守下列条件：
署名 — 您必须按照作者或者许可人指定的方式对作品进行署名。
相同方式共享 — 如果您改变、转换本作品或者以本作品为基础进行创作，
您只能采用与本协议相同的许可协议发布基于本作品的演绎作品。`