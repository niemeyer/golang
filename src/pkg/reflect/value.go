// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

import (
	"math"
	"runtime"
	"strconv"
	"unsafe"
)

const ptrSize = uintptr(unsafe.Sizeof((*byte)(nil)))
const cannotSet = "cannot set value obtained from unexported struct field"

// TODO: This will have to go away when
// the new gc goes in.
func memmove(adst, asrc unsafe.Pointer, n uintptr) {
	dst := uintptr(adst)
	src := uintptr(asrc)
	switch {
	case src < dst && src+n > dst:
		// byte copy backward
		// careful: i is unsigned
		for i := n; i > 0; {
			i--
			*(*byte)(unsafe.Pointer(dst + i)) = *(*byte)(unsafe.Pointer(src + i))
		}
	case (n|src|dst)&(ptrSize-1) != 0:
		// byte copy forward
		for i := uintptr(0); i < n; i++ {
			*(*byte)(unsafe.Pointer(dst + i)) = *(*byte)(unsafe.Pointer(src + i))
		}
	default:
		// word copy forward
		for i := uintptr(0); i < n; i += ptrSize {
			*(*uintptr)(unsafe.Pointer(dst + i)) = *(*uintptr)(unsafe.Pointer(src + i))
		}
	}
}

// Value is the reflection interface to a Go value.
//
// Not all methods apply to all kinds of values.  Restrictions,
// if any, are noted in the documentation for each method.
// Use the Kind method to find out the kind of value before
// calling kind-specific methods.  Calling a method
// inappropriate to the kind of type causes a run time panic.
//
// The zero Value represents no value.
// Its IsValid method returns false, its Kind method returns Invalid,
// its String method returns "<invalid Value>", and all other methods panic.
// Most functions and methods never return an invalid value.
// If one does, its documentation states the conditions explicitly.
//
// The fields of Value are exported so that clients can copy and
// pass Values around, but they should not be edited or inspected
// directly.  A future language change may make it possible not to
// export these fields while still keeping Values usable as values.
type Value struct {
	Internal [ivSize]byte
}

const ivSize = uintptr(unsafe.Sizeof(internalValue{}))

// A ValueError occurs when a Value method is invoked on
// a Value that does not support it.  Such cases are documented
// in the description of each method.
type ValueError struct {
	Method string
	Kind   Kind
}

func (e *ValueError) String() string {
	if e.Kind == 0 {
		return "reflect: call of " + e.Method + " on zero Value"
	}
	return "reflect: call of " + e.Method + " on " + e.Kind.String() + " Value"
}

// methodName returns the name of the calling method,
// assumed to be two stack frames above.
func methodName() string {
	pc, _, _, _ := runtime.Caller(2)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name()
}

// An iword is the word that would be stored in an
// interface to represent a given value v.  Specifically, if v is
// bigger than a pointer, its word is a pointer to v's data.
// Otherwise, its word is a zero uintptr with the data stored
// in the leading bytes.
type iword uintptr

func loadIword(p unsafe.Pointer, size uintptr) iword {
	// Run the copy ourselves instead of calling memmove
	// to avoid moving v to the heap.
	w := iword(0)
	switch size {
	default:
		panic("reflect: internal error: loadIword of " + strconv.Itoa(int(size)) + "-byte value")
	case 0:
	case 1:
		*(*uint8)(unsafe.Pointer(&w)) = *(*uint8)(p)
	case 2:
		*(*uint16)(unsafe.Pointer(&w)) = *(*uint16)(p)
	case 3:
		*(*[3]byte)(unsafe.Pointer(&w)) = *(*[3]byte)(p)
	case 4:
		*(*uint32)(unsafe.Pointer(&w)) = *(*uint32)(p)
	case 5:
		*(*[5]byte)(unsafe.Pointer(&w)) = *(*[5]byte)(p)
	case 6:
		*(*[6]byte)(unsafe.Pointer(&w)) = *(*[6]byte)(p)
	case 7:
		*(*[7]byte)(unsafe.Pointer(&w)) = *(*[7]byte)(p)
	case 8:
		*(*uint64)(unsafe.Pointer(&w)) = *(*uint64)(p)
	}
	return w
}

func storeIword(p unsafe.Pointer, w iword, size uintptr) {
	// Run the copy ourselves instead of calling memmove
	// to avoid moving v to the heap.
	switch size {
	default:
		panic("reflect: internal error: storeIword of " + strconv.Itoa(int(size)) + "-byte value")
	case 0:
	case 1:
		*(*uint8)(p) = *(*uint8)(unsafe.Pointer(&w))
	case 2:
		*(*uint16)(p) = *(*uint16)(unsafe.Pointer(&w))
	case 3:
		*(*[3]byte)(p) = *(*[3]byte)(unsafe.Pointer(&w))
	case 4:
		*(*uint32)(p) = *(*uint32)(unsafe.Pointer(&w))
	case 5:
		*(*[5]byte)(p) = *(*[5]byte)(unsafe.Pointer(&w))
	case 6:
		*(*[6]byte)(p) = *(*[6]byte)(unsafe.Pointer(&w))
	case 7:
		*(*[7]byte)(p) = *(*[7]byte)(unsafe.Pointer(&w))
	case 8:
		*(*uint64)(p) = *(*uint64)(unsafe.Pointer(&w))
	}
}

// emptyInterface is the header for an interface{} value.
type emptyInterface struct {
	typ  *runtime.Type
	word iword
}

// nonEmptyInterface is the header for a interface value with methods.
type nonEmptyInterface struct {
	// see ../runtime/iface.c:/Itab
	itab *struct {
		ityp   *runtime.Type // static interface type
		typ    *runtime.Type // dynamic concrete type
		link   unsafe.Pointer
		bad    int32
		unused int32
		fun    [100000]unsafe.Pointer // method table
	}
	word iword
}

// Regarding the implementation of Value:
//
// The Internal array is privately cast into an internalValue type.
// This is a workaround to the fact we cannot (yet?) hide fields
// while preserving the ability for clients to make copies of Values.
//
// If a Value is addressable (CanAddr returns true), then the Internal
// value holds a pointer to the actual field data, and Set stores through
// that pointer.  If a Value is not addressable (CanAddr returns false),
// then the Internal interface value holds the actual value.
//
// In addition to whether a value is addressable, we track whether it was
// obtained by using an unexported struct field.  Such values are allowed
// to be read, mainly to make fmt.Print more useful, but they are not
// allowed to be written.  We call such values read-only.
//
// A Value can be set (via the Set, SetUint, etc. methods) only if it is both
// addressable and not read-only.
//
// The two permission bits - addressable and read-only - are stored in
// the bottom two bits of the type pointer in the interface value.
//
//     ordinary value: Internal = value
//     addressable value: Internal = value, Internal.typ |= flagAddr
//     read-only value: Internal = value, Internal.typ |= flagRO
//     addressable, read-only value: Internal = value, Internal.typ |= flagAddr | flagRO
//
// It is important that the read-only values have the extra bit set
// (as opposed to using the bit to mean writable), because client code
// can grab the interface field and try to use it.  Having the extra bit
// set makes the type pointer compare not equal to any real type,
// so that a client cannot, say, write through v.Internal.(*int).
// The runtime routines that access interface types reject types with
// low bits set.
//
// If a Value fv = v.Method(i), then fv = v with the InternalMethod
// field set to i+1.  Methods are never addressable.
//
// All in all, this is a lot of effort just to avoid making this new API
// depend on a language change we'll probably do anyway, but
// it's helpful to keep the two separate, and much of the logic is
// necessary to implement the Interface method anyway.

const (
	flagAddr uint32 = 1 << iota // holds address of value
	flagRO                      // read-only
	flagMethod
	flagNilMethod
)

// An internalValue is the real representation of a Value.
// A zero Value is also a zero internalValue
type internalValue struct {
	typ  *commonType // type of value
	word iword
	rcvr iword
	flag uint32
}

// packValue returns a Value with the given flag bits, type, and interface word.
func packValue(flag uint32, typ *runtime.Type, word iword) Value {
	if typ == nil {
		panic("packValue")
	}
	t := toCommonType((*runtime.Type)(unsafe.Pointer(typ)))
	if t == nil {
		return Value{}
	}
	var iv internalValue
	iv.flag = flag
	iv.word = word
	if iv.flag&flagAddr != 0 {
		iv.typ = t.Elem().common()
	} else {
		iv.typ = t
	}
	return *(*Value)(unsafe.Pointer(&iv))
}

// valueFromAddr returns a Value using the given type and address.
func valueFromAddr(flag uint32, typ Type, addr unsafe.Pointer) Value {
	if flag&flagAddr != 0 {
		// Addressable, so the internal value is
		// an interface containing a pointer to the real value.
		return packValue(flag, PtrTo(typ).runtimeType(), iword(addr))
	}
	var w iword
	if n := typ.Size(); n <= ptrSize {
		// In line, so the interface word is the actual value.
		w = loadIword(addr, n)
	} else {
		// Not in line: the interface word is the address.
		w = iword(addr)
	}
	return packValue(flag, typ.runtimeType(), w)
}

// valueFromIword returns a Value using the given type and interface word.
func valueFromIword(flag uint32, typ Type, w iword) Value {
	if flag&flagAddr != 0 {
		panic("reflect: internal error: valueFromIword addressable")
	}
	return packValue(flag, typ.runtimeType(), w)
}

// indirect returns the interface word.  If the word is stored outside the
// internalValue for being assignable, indirect loads the external word and
// returns it instead.
func (iv internalValue) indirect() iword {
	if iv.flag&flagAddr != 0 && iv.typ.size <= ptrSize {
		return loadIword(unsafe.Pointer(iv.word), iv.typ.size)
	}
	return iv.word
}

// method returns an internalValue representing the ith method on
// the receiver iv.
func (iv internalValue) method(i int) internalValue {
	// If this Value is a method value (x.Method(i) for some Value x)
	// then we will invoke it using the interface form of the method,
	// which always passes the receiver as a single word.
	// Record that information.
	if iv.typ.Kind() == Interface {
		it := (*interfaceType)(unsafe.Pointer(iv.typ))
		if i < 0 || i >= len(it.methods) {
			panic("reflect: broken Value")
		}
		m := &it.methods[i]
		if m.pkgPath != nil {
			iv.flag |= flagRO
		}
		iv.typ = toCommonType(m.typ)
		iface := (*nonEmptyInterface)(unsafe.Pointer(iv.word))
		if iface.itab == nil {
			iv.word = 0
			iv.flag |= flagNilMethod
		} else {
			iv.word = iword(iface.itab.fun[i])
		}
		iv.rcvr = iface.word
	} else {
		ut := iv.typ.uncommon()
		if ut == nil || i < 0 || i >= len(ut.methods) {
			panic("reflect: broken Value")
		}
		m := &ut.methods[i]
		if m.pkgPath != nil {
			iv.flag |= flagRO
		}
		iv.rcvr = iv.indirect()
		iv.typ = toCommonType(m.mtyp)
		iv.word = iword(m.ifn)
	}
	iv.flag |= flagMethod
	iv.flag &^= flagAddr
	return iv
}


func (iv internalValue) mustBe(want Kind) {
	if iv.typ.Kind() != want {
		panic(&ValueError{methodName(), iv.typ.Kind()})
	}
}

func (iv internalValue) mustBeExported() {
	if iv.typ.Kind() == 0 {
		panic(&ValueError{methodName(), iv.typ.Kind()})
	}
	if iv.flag&flagRO != 0 {
		panic(methodName() + " using value obtained using unexported field")
	}
}

func (iv internalValue) mustBeAssignable() {
	if iv.typ.Kind() == 0 {
		panic(&ValueError{methodName(), iv.typ.Kind()})
	}
	// Assignable if addressable and not read-only.
	if iv.flag&flagRO != 0 {
		panic(methodName() + " using value obtained using unexported field")
	}
	if iv.flag&flagAddr == 0 {
		panic(methodName() + " using unaddressable value")
	}
}

// Addr returns a pointer value representing the address of v.
// It panics if CanAddr() returns false.
// Addr is typically used to obtain a pointer to a struct field
// or slice element in order to call a method that requires a
// pointer receiver.
func (v Value) Addr() Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	if iv.flag&flagAddr == 0 {
		panic("reflect.Value.Addr of unaddressable value")
	}
	return valueFromIword(iv.flag&flagRO, PtrTo(iv.typ.toType()), iv.word)
}

// Bool returns v's underlying value.
// It panics if v's kind is not Bool.
func (v Value) Bool() bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Bool)
	if iv.flag&flagAddr != 0 {
		return *(*bool)(unsafe.Pointer(iv.word))
	}
	return *(*bool)(unsafe.Pointer(&iv.word))
}

// CanAddr returns true if the value's address can be obtained with Addr.
// Such values are called addressable.  A value is addressable if it is
// an element of a slice, an element of an addressable array,
// a field of an addressable struct, or the result of dereferencing a pointer.
// If CanAddr returns false, calling Addr will panic.
func (v Value) CanAddr() bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	return iv.flag&flagAddr != 0
}

// CanSet returns true if the value of v can be changed.
// A Value can be changed only if it is addressable and was not
// obtained by the use of unexported struct fields.
// If CanSet returns false, calling Set or any type-specific
// setter (e.g., SetBool, SetInt64) will panic.
func (v Value) CanSet() bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	return iv.flag&(flagAddr|flagRO) == flagAddr
}

// Call calls the function v with the input arguments in.
// For example, if len(in) == 3, v.Call(in) represents the Go call v(in[0], in[1], in[2]).
// Call panics if v's Kind is not Func.
// It returns the output results as Values.
// As in Go, each input argument must be assignable to the
// type of the function's corresponding input parameter.
// If v is a variadic function, Call creates the variadic slice parameter
// itself, copying in the corresponding values.
func (v Value) Call(in []Value) []Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Func)
	iv.mustBeExported()
	return iv.call("Call", in)
}

// CallSlice calls the variadic function v with the input arguments in,
// assigning the slice in[len(in)-1] to v's final variadic argument.  
// For example, if len(in) == 3, v.Call(in) represents the Go call v(in[0], in[1], in[2]...).
// Call panics if v's Kind is not Func or if v is not variadic.
// It returns the output results as Values.
// As in Go, each input argument must be assignable to the
// type of the function's corresponding input parameter.
func (v Value) CallSlice(in []Value) []Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Func)
	iv.mustBeExported()
	return iv.call("CallSlice", in)
}

func (iv internalValue) call(method string, in []Value) []Value {
	mword := iv.indirect()
	if mword == 0 {
		if iv.flag&flagNilMethod != 0 {
			panic("reflect.Value.Call: call of method on nil interface value")
		}
		panic("reflect.Value.Call: call of nil function")
	}

	isSlice := method == "CallSlice"
	t := iv.typ
	n := t.NumIn()
	if isSlice {
		if !t.IsVariadic() {
			panic("reflect: CallSlice of non-variadic function")
		}
		if len(in) < n {
			panic("reflect: CallSlice with too few input arguments")
		}
		if len(in) > n {
			panic("reflect: CallSlice with too many input arguments")
		}
	} else {
		if t.IsVariadic() {
			n--
		}
		if len(in) < n {
			panic("reflect: Call with too few input arguments")
		}
		if !t.IsVariadic() && len(in) > n {
			panic("reflect: Call with too many input arguments")
		}
	}
	for _, x := range in {
		if x.Kind() == Invalid {
			panic("reflect: " + method + " using zero Value argument")
		}
	}
	for i := 0; i < n; i++ {
		if xt, targ := in[i].Type(), t.In(i); !xt.AssignableTo(targ) {
			panic("reflect: " + method + " using " + xt.String() + " as type " + targ.String())
		}
	}
	if !isSlice && t.IsVariadic() {
		// prepare slice for remaining values
		m := len(in) - n
		slice := MakeSlice(t.In(n), m, m)
		elem := t.In(n).Elem()
		for i := 0; i < m; i++ {
			x := in[n+i]
			if xt := x.Type(); !xt.AssignableTo(elem) {
				panic("reflect: cannot use " + xt.String() + " as type " + elem.String() + " in " + method)
			}
			slice.Index(i).Set(x)
		}
		origIn := in
		in = make([]Value, n+1)
		copy(in[:n], origIn)
		in[n] = slice
	}

	nin := len(in)
	if nin != t.NumIn() {
		panic("reflect.Value.Call: wrong argument count")
	}
	nout := t.NumOut()

	// Compute arg size & allocate.
	// This computation is 5g/6g/8g-dependent
	// and probably wrong for gccgo, but so
	// is most of this function.
	size := uintptr(0)
	if iv.flag&flagMethod != 0 {
		// extra word for interface value
		size += ptrSize
	}
	for i := 0; i < nin; i++ {
		tv := t.In(i)
		a := uintptr(tv.Align())
		size = (size + a - 1) &^ (a - 1)
		size += tv.Size()
	}
	size = (size + ptrSize - 1) &^ (ptrSize - 1)
	for i := 0; i < nout; i++ {
		tv := t.Out(i)
		a := uintptr(tv.Align())
		size = (size + a - 1) &^ (a - 1)
		size += tv.Size()
	}

	// size must be > 0 in order for &args[0] to be valid.
	// the argument copying is going to round it up to
	// a multiple of ptrSize anyway, so make it ptrSize to begin with.
	if size < ptrSize {
		size = ptrSize
	}

	// round to pointer size
	size = (size + ptrSize - 1) &^ (ptrSize - 1)

	// Copy into args.
	//
	// TODO(rsc): revisit when reference counting happens.
	// The values are holding up the in references for us,
	// but something must be done for the out references.
	// For now make everything look like a pointer by pretending
	// to allocate a []*int.
	args := make([]*int, size/ptrSize)
	ptr := uintptr(unsafe.Pointer(&args[0]))
	off := uintptr(0)
	if iv.flag&flagMethod != 0 {
		// Hard-wired first argument.
		*(*iword)(unsafe.Pointer(ptr)) = iv.rcvr
		off = ptrSize
	}
	for i, v := range in {
		iv := *(*internalValue)(unsafe.Pointer(&v))
		iv.mustBeExported()
		targ := t.In(i).(*commonType)
		a := uintptr(targ.align)
		off = (off + a - 1) &^ (a - 1)
		n := targ.size
		addr := unsafe.Pointer(ptr + off)
		if iv.typ != targ {
			iv = convertForAssignment("reflect.Value.Call", addr, targ, iv)
		}
		if iv.flag&flagAddr == 0 && iv.typ.size <= ptrSize {
			storeIword(addr, iv.word, n)
		} else {
			memmove(addr, unsafe.Pointer(iv.word), n)
		}
		off += n
	}
	off = (off + ptrSize - 1) &^ (ptrSize - 1)

	// Call.
	call(unsafe.Pointer(mword), unsafe.Pointer(ptr), uint32(size))

	// Copy return values out of args.
	//
	// TODO(rsc): revisit like above.
	ret := make([]Value, nout)
	for i := 0; i < nout; i++ {
		tv := t.Out(i)
		a := uintptr(tv.Align())
		off = (off + a - 1) &^ (a - 1)
		ret[i] = valueFromAddr(0, tv, unsafe.Pointer(ptr+off))
		off += tv.Size()
	}

	return ret
}

// Cap returns v's capacity.
// It panics if v's Kind is not Array, Chan, or Slice.
func (v Value) Cap() int {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Array:
		return iv.typ.Len()
	case Chan:
		return int(chancap(iv.indirect()))
	case Slice:
		return (*SliceHeader)(unsafe.Pointer(iv.word)).Cap
	}
	panic(&ValueError{"reflect.Value.Cap", iv.typ.Kind()})
}

// Close closes the channel v.
// It panics if v's Kind is not Chan.
func (v Value) Close() {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Chan)
	iv.mustBeExported()
	chanclose(iv.indirect())
}

// Complex returns v's underlying value, as a complex128.
// It panics if v's Kind is not Complex64 or Complex128
func (v Value) Complex() complex128 {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Complex64:
		if iv.flag&flagAddr == 0 && iv.typ.size <= ptrSize {
			return complex128(*(*complex64)(unsafe.Pointer(&iv.word)))
		}
		return complex128(*(*complex64)(unsafe.Pointer(iv.word)))
	case Complex128:
		return *(*complex128)(unsafe.Pointer(iv.word))
	}
	panic(&ValueError{"reflect.Value.Complex", iv.typ.Kind()})
}

// Elem returns the value that the interface v contains
// or that the pointer v points to.
// It panics if v's Kind is not Interface or Ptr.
// It returns the zero Value if v is nil.
func (v Value) Elem() Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	return iv.Elem()
}

func (iv internalValue) Elem() Value {
	switch iv.typ.Kind() {
	case Interface:
		// Empty interface and non-empty interface have different layouts.
		// Convert to empty interface.
		var eface emptyInterface
		if iv.typ.NumMethod() == 0 {
			eface = *(*emptyInterface)(unsafe.Pointer(iv.word))
		} else {
			iface := (*nonEmptyInterface)(unsafe.Pointer(iv.word))
			if iface.itab != nil {
				eface.typ = iface.itab.typ
			}
			eface.word = iface.word
		}
		if eface.typ == nil {
			return Value{}
		}
		return valueFromIword(iv.flag&flagRO, toType(eface.typ), eface.word)

	case Ptr:
		// The returned value's address is v's value.
		addr := iv.indirect()
		if addr == 0 {
			return Value{}
		}
		return valueFromAddr(iv.flag&flagRO|flagAddr, iv.typ.Elem(), unsafe.Pointer(addr))
	}
	panic(&ValueError{"reflect.Value.Elem", iv.typ.Kind()})
}

// Field returns the i'th field of the struct v.
// It panics if v's Kind is not Struct or i is out of range.
func (v Value) Field(i int) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Struct)
	t := iv.typ.toType()
	if i < 0 || i >= t.NumField() {
		panic("reflect: Field index out of range")
	}
	f := t.Field(i)

	// Inherit permission bits from v.
	flag := iv.flag
	// Using an unexported field forces flagRO.
	if f.PkgPath != "" {
		flag |= flagRO
	}
	return valueFromValueOffset(flag, f.Type, iv, f.Offset)
}

// valueFromValueOffset returns a sub-value of outer
// (outer is an array or a struct) with the given flag and type
// starting at the given byte offset into outer.
func valueFromValueOffset(flag uint32, typ Type, outer internalValue, offset uintptr) Value {
	if outer.flag&flagAddr != 0 || outer.typ.size > ptrSize {
		return valueFromAddr(flag, typ, unsafe.Pointer(uintptr(outer.word)+offset))
	}

	// outer is so tiny it is in line.
	// We have to use outer.word and derive
	// the new word (it cannot possibly be bigger).
	// In line, so not addressable.
	if flag&flagAddr != 0 {
		panic("reflect: internal error: misuse of valueFromValueOffset")
	}
	b := *(*[ptrSize]byte)(unsafe.Pointer(&outer.word))
	for i := uintptr(0); i < typ.Size(); i++ {
		b[i] = b[offset+i]
	}
	for i := typ.Size(); i < ptrSize; i++ {
		b[i] = 0
	}
	w := *(*iword)(unsafe.Pointer(&b))
	return valueFromIword(flag, typ, w)
}

// FieldByIndex returns the nested field corresponding to index.
// It panics if v's Kind is not struct.
func (v Value) FieldByIndex(index []int) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Struct)
	for i, x := range index {
		if i > 0 {
			if v.Kind() == Ptr && v.Elem().Kind() == Struct {
				v = v.Elem()
			}
		}
		v = v.Field(x)
	}
	return v
}

// FieldByName returns the struct field with the given name.
// It returns the zero Value if no field was found.
// It panics if v's Kind is not struct.
func (v Value) FieldByName(name string) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Struct)
	if f, ok := iv.typ.FieldByName(name); ok {
		return v.FieldByIndex(f.Index)
	}
	return Value{}
}

// FieldByNameFunc returns the struct field with a name
// that satisfies the match function.
// It panics if v's Kind is not struct.
// It returns the zero Value if no field was found.
func (v Value) FieldByNameFunc(match func(string) bool) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Struct)
	if f, ok := v.Type().FieldByNameFunc(match); ok {
		return v.FieldByIndex(f.Index)
	}
	return Value{}
}

// Float returns v's underlying value, as an float64.
// It panics if v's Kind is not Float32 or Float64
func (v Value) Float() float64 {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Float32:
		if iv.flag&flagAddr == 0 {
			return float64(*(*float32)(unsafe.Pointer(&iv.word)))
		}
		return float64(*(*float32)(unsafe.Pointer(iv.word)))
	case Float64:
		// If the pointer width can fit an entire float64,
		// the value is in line when stored in an interface.
		if iv.flag&flagAddr == 0 && iv.typ.size <= ptrSize {
			return *(*float64)(unsafe.Pointer(&iv.word))
		}
		// Otherwise we have a pointer.
		return *(*float64)(unsafe.Pointer(iv.word))
	}
	panic(&ValueError{"reflect.Value.Float", iv.typ.Kind()})
}

// Index returns v's i'th element.
// It panics if v's Kind is not Array or Slice or i is out of range.
func (v Value) Index(i int) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	default:
		panic(&ValueError{"reflect.Value.Index", iv.typ.Kind()})
	case Array:
		flag := iv.flag // element flag same as overall array
		t := iv.typ.toType()
		if i < 0 || i > t.Len() {
			panic("reflect: array index out of range")
		}
		typ := t.Elem()
		return valueFromValueOffset(flag, typ, iv, uintptr(i)*typ.Size())

	case Slice:
		// Element flag same as Elem of Ptr.
		// Addressable, possibly read-only.
		flag := iv.flag&flagRO | flagAddr
		s := (*SliceHeader)(unsafe.Pointer(iv.word))
		if i < 0 || i >= s.Len {
			panic("reflect: slice index out of range")
		}
		typ := iv.typ.Elem()
		addr := unsafe.Pointer(s.Data + uintptr(i)*typ.Size())
		return valueFromAddr(flag, typ, addr)
	}

	panic("not reached")
}

// Int returns v's underlying value, as an int64.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64.
func (v Value) Int() int64 {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	w := iv.indirect()
	switch iv.typ.Kind() {
	case Int:
		return int64(*(*int)(unsafe.Pointer(&w)))
	case Int8:
		return int64(*(*int8)(unsafe.Pointer(&w)))
	case Int16:
		return int64(*(*int16)(unsafe.Pointer(&w)))
	case Int32:
		return int64(*(*int32)(unsafe.Pointer(&w)))
	case Int64:
		return *(*int64)(unsafe.Pointer(&w))
	}
	panic(&ValueError{"reflect.Value.Int", iv.typ.Kind()})
}

// CanInterface returns true if Interface can be used without panicking.
func (v Value) CanInterface() bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	if iv.typ == nil {
		panic(&ValueError{"reflect.Value.CanInterface", iv.typ.Kind()})
	}
	// TODO(rsc): Check flagRO too.  Decide what to do about asking for
	// interface for a value obtained via an unexported field.
	// If the field were of a known type, say chan int or *sync.Mutex,
	// the caller could interfere with the data after getting the
	// interface.  But fmt.Print depends on being able to look.
	// Now that reflect is more efficient the special cases in fmt
	// might be less important.
	return iv.flag&flagMethod == 0
}

// Interface returns v's value as an interface{}.
// If v is a method obtained by invoking Value.Method
// (as opposed to Type.Method), Interface cannot return an
// interface value, so it panics.
func (v Value) Interface() interface{} {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	return iv.Interface()
}

func (iv internalValue) Interface() interface{} {
	if iv.flag&flagMethod != 0 {
		panic("reflect.Value.Interface: cannot create interface value for method with bound receiver")
	}
	/*
		if v.flag()&noExport != 0 {
			panic("reflect.Value.Interface: cannot return value obtained from unexported struct field")
		}
	*/

	if iv.typ.Kind() == Interface {
		// Special case: return the element inside the interface.
		// Won't recurse further because an interface cannot contain an interface.
		if iv.IsNil() {
			return nil
		}
		return iv.Elem().Interface()
	}

	// Non-interface value.
	var eface emptyInterface
	eface.typ = iv.typ.runtimeType()
	eface.word = iv.indirect()
	return *(*interface{})(unsafe.Pointer(&eface))
}

// InterfaceData returns the interface v's value as a uintptr pair.
// It panics if v's Kind is not Interface.
func (v Value) InterfaceData() [2]uintptr {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Interface)
	// We treat this as a read operation, so we allow
	// it even for unexported data, because the caller
	// has to import "unsafe" to turn it into something
	// that can be abused.
	return *(*[2]uintptr)(unsafe.Pointer(iv.word))
}

// IsNil returns true if v is a nil value.
// It panics if v's Kind is not Chan, Func, Interface, Map, Ptr, or Slice.
func (v Value) IsNil() bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	return iv.IsNil()
}

func (iv internalValue) IsNil() bool {
	switch iv.typ.Kind() {
	case Chan, Func, Map, Ptr:
		if iv.flag&flagMethod != 0 {
			panic("reflect: IsNil of method Value")
		}
		return iv.indirect() == 0
	case Interface, Slice:
		// Both interface and slice are nil if first word is 0.
		return *(*uintptr)(unsafe.Pointer(iv.word)) == 0
	}
	panic(&ValueError{"reflect.Value.IsNil", iv.typ.Kind()})
}

// IsValid returns true if v represents a value.
// It returns false if v is the zero Value.
// If IsValid returns false, all other methods except String panic.
// Most functions and methods never return an invalid value.
// If one does, its documentation states the conditions explicitly.
func (v Value) IsValid() bool {
	// Reduce copying knowing typ is first in the struct.
	typ := (*(**commonType)(unsafe.Pointer(&v)))
	return typ != nil
}

// Kind returns v's Kind.
// If v is the zero Value (IsValid returns false), Kind returns Invalid.
func (v Value) Kind() Kind {
	// Reduce copying knowing typ is first in the struct.
	typ := (*(**commonType)(unsafe.Pointer(&v)))
	return typ.Kind()
}

// Len returns v's length.
// It panics if v's Kind is not Array, Chan, Map, or Slice.
func (v Value) Len() int {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Array:
		return iv.typ.Len()
	case Chan:
		return int(chanlen(iv.indirect()))
	case Map:
		return int(maplen(iv.indirect()))
	case Slice:
		return (*SliceHeader)(unsafe.Pointer(iv.word)).Len
	}
	panic(&ValueError{"reflect.Value.Len", iv.typ.Kind()})
}

// MapIndex returns the value associated with key in the map v.
// It panics if v's Kind is not Map.
// It returns the zero Value if key is not found in the map or if v represents a nil map.
// As in Go, the key's value must be assignable to the map's key type.
func (v Value) MapIndex(key Value) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Map)
	typ := iv.typ.toType()

	// Do not require ikey to be exported, so that DeepEqual
	// and other programs can use all the keys returned by
	// MapKeys as arguments to MapIndex.  If either the map
	// or the key is unexported, though, the result will be
	// considered unexported.

	ikey := *(*internalValue)(unsafe.Pointer(&key))
	keyt := typ.Key()
	if ikey.typ != keyt.(*commonType) {
		ikey = convertForAssignment("reflect.Value.MapIndex", nil, keyt, ikey)
	}
	m := iv.indirect()
	if m == 0 {
		return Value{}
	}

	flag := (iv.flag | ikey.flag) & flagRO
	elemType := typ.Elem()
	elemWord, ok := mapaccess(m, ikey.word)
	if !ok {
		return Value{}
	}
	return valueFromIword(flag, elemType, elemWord)
}

// MapKeys returns a slice containing all the keys present in the map,
// in unspecified order.
// It panics if v's Kind is not Map.
// It returns an empty slice if v represents a nil map.
func (v Value) MapKeys() []Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Map)
	keyType := iv.typ.Key()

	flag := iv.flag & flagRO
	m := iv.indirect()
	mlen := int32(0)
	if m != 0 {
		mlen = maplen(m)
	}
	it := mapiterinit(m)
	a := make([]Value, mlen)
	var i int
	for i = 0; i < len(a); i++ {
		keyWord, ok := mapiterkey(it)
		if !ok {
			break
		}
		a[i] = valueFromIword(flag, keyType, keyWord)
		mapiternext(it)
	}
	return a[:i]
}

// Method returns a function value corresponding to v's i'th method.
// The arguments to a Call on the returned function should not include
// a receiver; the returned function will always use v as the receiver.
// Method panics if i is out of range.
func (v Value) Method(i int) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	if iv.typ == nil {
		panic(&ValueError{"reflect.Value.Method", Invalid})
	}
	if i < 0 || i >= iv.typ.NumMethod() {
		panic("reflect: Method index out of range")
	}
	mv := Value{}
	*(*internalValue)(unsafe.Pointer(&mv)) = iv.method(i)
	return mv
}

// NumField returns the number of fields in the struct v.
// It panics if v's Kind is not Struct.
func (v Value) NumField() int {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Struct)
	return iv.typ.NumField()
}

// OverflowComplex returns true if the complex128 x cannot be represented by v's type.
// It panics if v's Kind is not Complex64 or Complex128.
func (v Value) OverflowComplex(x complex128) bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Complex64:
		return overflowFloat32(real(x)) || overflowFloat32(imag(x))
	case Complex128:
		return false
	}
	panic(&ValueError{"reflect.Value.OverflowComplex", iv.typ.Kind()})
}

// OverflowFloat returns true if the float64 x cannot be represented by v's type.
// It panics if v's Kind is not Float32 or Float64.
func (v Value) OverflowFloat(x float64) bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Float32:
		return overflowFloat32(x)
	case Float64:
		return false
	}
	panic(&ValueError{"reflect.Value.OverflowFloat", iv.typ.Kind()})
}

func overflowFloat32(x float64) bool {
	if x < 0 {
		x = -x
	}
	return math.MaxFloat32 <= x && x <= math.MaxFloat64
}

// OverflowInt returns true if the int64 x cannot be represented by v's type.
// It panics if v's Kind is not Int, Int8, int16, Int32, or Int64.
func (v Value) OverflowInt(x int64) bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Int, Int8, Int16, Int32, Int64:
		bitSize := iv.typ.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic(&ValueError{"reflect.Value.OverflowInt", iv.typ.Kind()})
}

// OverflowUint returns true if the uint64 x cannot be represented by v's type.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64.
func (v Value) OverflowUint(x uint64) bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Uint, Uintptr, Uint8, Uint16, Uint32, Uint64:
		bitSize := iv.typ.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic(&ValueError{"reflect.Value.OverflowUint", iv.typ.Kind()})
}

// Pointer returns v's value as a uintptr.
// It returns uintptr instead of unsafe.Pointer so that
// code using reflect cannot obtain unsafe.Pointers
// without importing the unsafe package explicitly.
// It panics if v's Kind is not Chan, Func, Map, Ptr, Slice, or UnsafePointer.
func (v Value) Pointer() uintptr {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Chan, Func, Map, Ptr, UnsafePointer:
		if iv.typ.Kind() == Func && iv.flag&flagMethod != 0 {
			panic("reflect.Value.Pointer of method Value")
		}
		return uintptr(iv.indirect())
	case Slice:
		return (*SliceHeader)(unsafe.Pointer(iv.word)).Data
	}
	panic(&ValueError{"reflect.Value.Pointer", iv.typ.Kind()})
}

// Recv receives and returns a value from the channel v.
// It panics if v's Kind is not Chan.
// The receive blocks until a value is ready.
// The boolean value ok is true if the value x corresponds to a send
// on the channel, false if it is a zero value received because the channel is closed.
func (v Value) Recv() (x Value, ok bool) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Chan)
	iv.mustBeExported()
	return iv.recv(false)
}

// internal recv, possibly non-blocking (nb)
func (iv internalValue) recv(nb bool) (val Value, ok bool) {
	t := iv.typ.toType()
	if t.ChanDir()&RecvDir == 0 {
		panic("recv on send-only channel")
	}
	ch := iv.indirect()
	if ch == 0 {
		panic("recv on nil channel")
	}
	valWord, selected, ok := chanrecv(ch, nb)
	if selected {
		val = valueFromIword(0, t.Elem(), valWord)
	}
	return
}

// Send sends x on the channel v.
// It panics if v's kind is not Chan or if x's type is not the same type as v's element type.
// As in Go, x's value must be assignable to the channel's element type.
func (v Value) Send(x Value) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Chan)
	iv.mustBeExported()
	iv.send(x, false)
}

// internal send, possibly non-blocking
func (iv internalValue) send(x Value, nb bool) (selected bool) {
	t := iv.typ.toType()
	if t.ChanDir()&SendDir == 0 {
		panic("send on recv-only channel")
	}

	ix := *(*internalValue)(unsafe.Pointer(&x))
	ix.mustBeExported() // do not let unexported x leak
	et := t.Elem()
	if ix.typ != et.(*commonType) {
		ix = convertForAssignment("reflect.Value.Send", nil, et, ix)
	}

	ch := iv.indirect()
	if ch == 0 {
		panic("send on nil channel")
	}
	return chansend(ch, ix.indirect(), nb)
}

// Set assigns x to the value v.
// It panics if CanSet returns false.
// As in Go, x's value must be assignable to v's type.
func (v Value) Set(x Value) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	ix := *(*internalValue)(unsafe.Pointer(&x))

	iv.mustBeAssignable()
	ix.mustBeExported() // do not let unexported x leak

	if ix.typ != iv.typ {
		ix = convertForAssignment("reflect.Set", unsafe.Pointer(iv.word), iv.typ, ix)
	}

	n := ix.typ.size
	if n <= ptrSize {
		storeIword(unsafe.Pointer(iv.word), ix.indirect(), n)
	} else {
		memmove(unsafe.Pointer(iv.word), unsafe.Pointer(ix.word), n)
	}
}

// SetBool sets v's underlying value.
// It panics if v's Kind is not Bool or if CanSet() is false.
func (v Value) SetBool(x bool) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	iv.mustBe(Bool)
	*(*bool)(unsafe.Pointer(iv.word)) = x
}

// SetComplex sets v's underlying value to x.
// It panics if v's Kind is not Complex64 or Complex128, or if CanSet() is false.
func (v Value) SetComplex(x complex128) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	switch iv.typ.Kind() {
	default:
		panic(&ValueError{"reflect.Value.SetComplex", iv.typ.Kind()})
	case Complex64:
		*(*complex64)(unsafe.Pointer(iv.word)) = complex64(x)
	case Complex128:
		*(*complex128)(unsafe.Pointer(iv.word)) = x
	}
}

// SetFloat sets v's underlying value to x.
// It panics if v's Kind is not Float32 or Float64, or if CanSet() is false.
func (v Value) SetFloat(x float64) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	switch iv.typ.Kind() {
	default:
		panic(&ValueError{"reflect.Value.SetFloat", iv.typ.Kind()})
	case Float32:
		*(*float32)(unsafe.Pointer(iv.word)) = float32(x)
	case Float64:
		*(*float64)(unsafe.Pointer(iv.word)) = x
	}
}

// SetInt sets v's underlying value to x.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64, or if CanSet() is false.
func (v Value) SetInt(x int64) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	switch iv.typ.Kind() {
	default:
		panic(&ValueError{"reflect.Value.SetInt", iv.typ.Kind()})
	case Int:
		*(*int)(unsafe.Pointer(iv.word)) = int(x)
	case Int8:
		*(*int8)(unsafe.Pointer(iv.word)) = int8(x)
	case Int16:
		*(*int16)(unsafe.Pointer(iv.word)) = int16(x)
	case Int32:
		*(*int32)(unsafe.Pointer(iv.word)) = int32(x)
	case Int64:
		*(*int64)(unsafe.Pointer(iv.word)) = x
	}
}

// SetLen sets v's length to n.
// It panics if v's Kind is not Slice.
func (v Value) SetLen(n int) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	iv.mustBe(Slice)
	s := (*SliceHeader)(unsafe.Pointer(iv.word))
	if n < 0 || n > int(s.Cap) {
		panic("reflect: slice length out of range in SetLen")
	}
	s.Len = n
}

// SetMapIndex sets the value associated with key in the map v to val.
// It panics if v's Kind is not Map.
// If val is the zero Value, SetMapIndex deletes the key from the map.
// As in Go, key's value must be assignable to the map's key type,
// and val's value must be assignable to the map's value type.
func (v Value) SetMapIndex(key, val Value) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	ikey := *(*internalValue)(unsafe.Pointer(&key))
	ival := *(*internalValue)(unsafe.Pointer(&val))

	iv.mustBe(Map)
	iv.mustBeExported()

	ikey.mustBeExported()
	keyt := iv.typ.Key()
	if ikey.typ != keyt.(*commonType) {
		ikey = convertForAssignment("reflect.Value.SetMapIndex", nil, keyt, ikey)
	}

	if ival.typ != nil {
		ival.mustBeExported()
		valt := iv.typ.Elem()
		if ival.typ != valt.(*commonType) {
			ival = convertForAssignment("reflect.Value.SetMapIndex", nil, valt, ival)
		}
	}

	mapassign(iv.indirect(), ikey.indirect(), ival.indirect(), ival.typ != nil)
}

// SetUint sets v's underlying value to x.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64, or if CanSet() is false.
func (v Value) SetUint(x uint64) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	switch iv.typ.Kind() {
	default:
		panic(&ValueError{"reflect.Value.SetUint", iv.typ.Kind()})
	case Uint:
		*(*uint)(unsafe.Pointer(iv.word)) = uint(x)
	case Uint8:
		*(*uint8)(unsafe.Pointer(iv.word)) = uint8(x)
	case Uint16:
		*(*uint16)(unsafe.Pointer(iv.word)) = uint16(x)
	case Uint32:
		*(*uint32)(unsafe.Pointer(iv.word)) = uint32(x)
	case Uint64:
		*(*uint64)(unsafe.Pointer(iv.word)) = x
	case Uintptr:
		*(*uintptr)(unsafe.Pointer(iv.word)) = uintptr(x)
	}
}

// SetPointer sets the unsafe.Pointer value v to x.
// It panics if v's Kind is not UnsafePointer.
func (v Value) SetPointer(x unsafe.Pointer) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	iv.mustBe(UnsafePointer)
	*(*unsafe.Pointer)(unsafe.Pointer(iv.word)) = x
}

// SetString sets v's underlying value to x.
// It panics if v's Kind is not String or if CanSet() is false.
func (v Value) SetString(x string) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBeAssignable()
	iv.mustBe(String)
	*(*string)(unsafe.Pointer(iv.word)) = x
}

// Slice returns a slice of v.
// It panics if v's Kind is not Array or Slice.
func (v Value) Slice(beg, end int) Value {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	if iv.typ.Kind() != Array && iv.typ.Kind() != Slice {
		panic(&ValueError{"reflect.Value.Slice", iv.typ.Kind()})
	}
	cap := v.Cap()
	if beg < 0 || end < beg || end > cap {
		panic("reflect.Value.Slice: slice index out of bounds")
	}
	var typ Type
	var base uintptr
	switch iv.typ.Kind() {
	case Array:
		if iv.flag&flagAddr == 0 {
			panic("reflect.Value.Slice: slice of unaddressable array")
		}
		typ = toType((*arrayType)(unsafe.Pointer(iv.typ)).slice)
		base = uintptr(unsafe.Pointer(iv.word))
	case Slice:
		typ = iv.typ.toType()
		base = (*SliceHeader)(unsafe.Pointer(iv.word)).Data
	}
	s := new(SliceHeader)
	s.Data = base + uintptr(beg)*typ.Elem().Size()
	s.Len = end - beg
	s.Cap = cap - beg
	return valueFromAddr(iv.flag&flagRO, typ, unsafe.Pointer(s))
}

// String returns the string v's underlying value, as a string.
// String is a special case because of Go's String method convention.
// Unlike the other getters, it does not panic if v's Kind is not String.
// Instead, it returns a string of the form "<T value>" where T is v's type.
func (v Value) String() string {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	switch iv.typ.Kind() {
	case Invalid:
		return "<invalid Value>"
	case String:
		return *(*string)(unsafe.Pointer(iv.word))
	}
	return "<" + iv.typ.String() + " Value>"
}

// TryRecv attempts to receive a value from the channel v but will not block.
// It panics if v's Kind is not Chan.
// If the receive cannot finish without blocking, x is the zero Value.
// The boolean ok is true if the value x corresponds to a send
// on the channel, false if it is a zero value received because the channel is closed.
func (v Value) TryRecv() (x Value, ok bool) {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Chan)
	iv.mustBeExported()
	return iv.recv(true)
}

// TrySend attempts to send x on the channel v but will not block.
// It panics if v's Kind is not Chan.
// It returns true if the value was sent, false otherwise.
// As in Go, x's value must be assignable to the channel's element type.
func (v Value) TrySend(x Value) bool {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	iv.mustBe(Chan)
	iv.mustBeExported()
	return iv.send(x, true)
}

// Type returns v's type.
func (v Value) Type() Type {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	t := iv.typ
	if t == nil {
		panic(&ValueError{"reflect.Value.Type", Invalid})
	}
	return t.toType()
}

// Uint returns v's underlying value, as a uint64.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64.
func (v Value) Uint() uint64 {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	w := iv.indirect()
	switch iv.typ.Kind() {
	case Uint:
		return uint64(*(*uint)(unsafe.Pointer(&w)))
	case Uint8:
		return uint64(*(*uint8)(unsafe.Pointer(&w)))
	case Uint16:
		return uint64(*(*uint16)(unsafe.Pointer(&w)))
	case Uint32:
		return uint64(*(*uint32)(unsafe.Pointer(&w)))
	case Uintptr:
		return uint64(*(*uintptr)(unsafe.Pointer(&w)))
	case Uint64:
		return uint64(*(*uint64)(unsafe.Pointer(&w)))
	}
	panic(&ValueError{"reflect.Value.Uint", iv.typ.Kind()})
}

// UnsafeAddr returns a pointer to v's data.
// It is for advanced clients that also import the "unsafe" package.
// It panics if v is not addressable.
func (v Value) UnsafeAddr() uintptr {
	iv := *(*internalValue)(unsafe.Pointer(&v))
	if iv.typ == nil {
		panic(&ValueError{"reflect.Value.UnsafeAddr", iv.typ.Kind()})
	}
	if iv.flag&flagAddr == 0 {
		panic("reflect.Value.UnsafeAddr of unaddressable value")
	}
	return uintptr(unsafe.Pointer(iv.word))
}

// StringHeader is the runtime representation of a string.
// It cannot be used safely or portably.
type StringHeader struct {
	Data uintptr
	Len  int
}

// SliceHeader is the runtime representation of a slice.
// It cannot be used safely or portably.
type SliceHeader struct {
	Data uintptr
	Len  int
	Cap  int
}

func typesMustMatch(what string, t1, t2 Type) {
	if t1 != t2 {
		panic("reflect: " + what + ": " + t1.String() + " != " + t2.String())
	}
}

// grow grows the slice s so that it can hold extra more values, allocating
// more capacity if needed. It also returns the old and new slice lengths.
func grow(s Value, extra int) (Value, int, int) {
	i0 := s.Len()
	i1 := i0 + extra
	if i1 < i0 {
		panic("reflect.Append: slice overflow")
	}
	m := s.Cap()
	if i1 <= m {
		return s.Slice(0, i1), i0, i1
	}
	if m == 0 {
		m = extra
	} else {
		for m < i1 {
			if i0 < 1024 {
				m += m
			} else {
				m += m / 4
			}
		}
	}
	t := MakeSlice(s.Type(), i1, m)
	Copy(t, s)
	return t, i0, i1
}

// Append appends the values x to a slice s and returns the resulting slice.
// As in Go, each x's value must be assignable to the slice's element type.
func Append(s Value, x ...Value) Value {
	siv := *(*internalValue)(unsafe.Pointer(&s))
	siv.mustBe(Slice)
	s, i0, i1 := grow(s, len(x))
	for i, j := i0, 0; i < i1; i, j = i+1, j+1 {
		s.Index(i).Set(x[j])
	}
	return s
}

// AppendSlice appends a slice t to a slice s and returns the resulting slice.
// The slices s and t must have the same element type.
func AppendSlice(s, t Value) Value {
	siv := *(*internalValue)(unsafe.Pointer(&s))
	tiv := *(*internalValue)(unsafe.Pointer(&t))
	siv.mustBe(Slice)
	tiv.mustBe(Slice)
	typesMustMatch("reflect.AppendSlice", s.Type().Elem(), t.Type().Elem())
	s, i0, i1 := grow(s, t.Len())
	Copy(s.Slice(i0, i1), t)
	return s
}

// Copy copies the contents of src into dst until either
// dst has been filled or src has been exhausted.
// It returns the number of elements copied.
// Dst and src each must have kind Slice or Array, and
// dst and src must have the same element type.
func Copy(dst, src Value) int {
	idst := *(*internalValue)(unsafe.Pointer(&dst))
	isrc := *(*internalValue)(unsafe.Pointer(&src))

	dkind := idst.typ.Kind()
	skind := isrc.typ.Kind()

	if dkind != Array && dkind != Slice {
		panic(&ValueError{"reflect.Copy", dkind})
	}
	if dkind == Array {
		idst.mustBeAssignable()
	}
	idst.mustBeExported()
	if skind != Array && skind != Slice {
		panic(&ValueError{"reflect.Copy", skind})
	}
	isrc.mustBeExported()

	de := idst.typ.Elem()
	se := isrc.typ.Elem()
	typesMustMatch("reflect.Copy", de, se)

	n := dst.Len()
	if sn := src.Len(); n > sn {
		n = sn
	}

	// If sk is an in-line array, cannot take its address.
	// Instead, copy element by element.
	if isrc.flag&flagAddr == 0 && isrc.typ.size <= ptrSize {
		for i := 0; i < n; i++ {
			dst.Index(i).Set(src.Index(i))
		}
		return n
	}

	// Copy via memmove.
	var da, sa unsafe.Pointer
	if dkind == Array {
		da = unsafe.Pointer(idst.word)
	} else {
		da = unsafe.Pointer((*SliceHeader)(unsafe.Pointer(idst.word)).Data)
	}
	if skind == Array {
		sa = unsafe.Pointer(isrc.word)
	} else {
		sa = unsafe.Pointer((*SliceHeader)(unsafe.Pointer(isrc.word)).Data)
	}
	memmove(da, sa, uintptr(n)*de.Size())
	return n
}

/*
 * constructors
 */

// MakeSlice creates a new zero-initialized slice value
// for the specified slice type, length, and capacity.
func MakeSlice(typ Type, len, cap int) Value {
	if typ.Kind() != Slice {
		panic("reflect: MakeSlice of non-slice type")
	}
	s := &SliceHeader{
		Data: uintptr(unsafe.NewArray(typ.Elem(), cap)),
		Len:  len,
		Cap:  cap,
	}
	return valueFromAddr(0, typ, unsafe.Pointer(s))
}

// MakeChan creates a new channel with the specified type and buffer size.
func MakeChan(typ Type, buffer int) Value {
	if typ.Kind() != Chan {
		panic("reflect: MakeChan of non-chan type")
	}
	if buffer < 0 {
		panic("MakeChan: negative buffer size")
	}
	if typ.ChanDir() != BothDir {
		panic("MakeChan: unidirectional channel type")
	}
	ch := makechan(typ.runtimeType(), uint32(buffer))
	return valueFromIword(0, typ, ch)
}

// MakeMap creates a new map of the specified type.
func MakeMap(typ Type) Value {
	if typ.Kind() != Map {
		panic("reflect: MakeMap of non-map type")
	}
	m := makemap(typ.runtimeType())
	return valueFromIword(0, typ, m)
}

// Indirect returns the value that v points to.
// If v is a nil pointer, Indirect returns a nil Value.
// If v is not a pointer, Indirect returns v.
func Indirect(v Value) Value {
	if v.Kind() != Ptr {
		return v
	}
	return v.Elem()
}

// ValueOf returns a new Value initialized to the concrete value
// stored in the interface i.  ValueOf(nil) returns the zero Value.
func ValueOf(i interface{}) Value {
	if i == nil {
		return Value{}
	}
	// For an interface value with the noAddr bit set,
	// the representation is identical to an empty interface.
	eface := *(*emptyInterface)(unsafe.Pointer(&i))
	return packValue(0, eface.typ, eface.word)
}

// Zero returns a Value representing a zero value for the specified type.
// The result is different from the zero value of the Value struct,
// which represents no value at all.
// For example, Zero(TypeOf(42)) returns a Value with Kind Int and value 0.
func Zero(typ Type) Value {
	if typ == nil {
		panic("reflect: Zero(nil)")
	}
	if typ.Size() <= ptrSize {
		return valueFromIword(0, typ, 0)
	}
	return valueFromAddr(0, typ, unsafe.New(typ))
}

// New returns a Value representing a pointer to a new zero value
// for the specified type.  That is, the returned Value's Type is PtrTo(t).
func New(typ Type) Value {
	if typ == nil {
		panic("reflect: New(nil)")
	}
	ptr := unsafe.New(typ)
	return valueFromIword(0, PtrTo(typ), iword(ptr))
}

// convertForAssignment 
func convertForAssignment(what string, addr unsafe.Pointer, dst Type, iv internalValue) internalValue {
	if iv.flag&flagMethod != 0 {
		panic(what + ": cannot assign method value to type " + dst.String())
	}

	dst1 := dst.(*commonType)
	if directlyAssignable(dst1, iv.typ) {
		// Overwrite type so that they match.
		// Same memory layout, so no harm done.
		iv.typ = dst1
		return iv
	}
	if implements(dst1, iv.typ) {
		if addr == nil {
			addr = unsafe.Pointer(new(interface{}))
		}
		x := iv.Interface()
		if dst.NumMethod() == 0 {
			*(*interface{})(addr) = x
		} else {
			ifaceE2I(dst1.runtimeType(), x, addr)
		}
		iv.word = iword(addr)
		iv.typ = dst1
		return iv
	}

	// Failed.
	panic(what + ": value of type " + iv.typ.String() + " is not assignable to type " + dst.String())
}

// implemented in ../pkg/runtime
func chancap(ch iword) int32
func chanclose(ch iword)
func chanlen(ch iword) int32
func chanrecv(ch iword, nb bool) (val iword, selected, received bool)
func chansend(ch iword, val iword, nb bool) bool

func makechan(typ *runtime.Type, size uint32) (ch iword)
func makemap(t *runtime.Type) iword
func mapaccess(m iword, key iword) (val iword, ok bool)
func mapassign(m iword, key, val iword, ok bool)
func mapiterinit(m iword) *byte
func mapiterkey(it *byte) (key iword, ok bool)
func mapiternext(it *byte)
func maplen(m iword) int32

func call(fn, arg unsafe.Pointer, n uint32)
func ifaceE2I(t *runtime.Type, src interface{}, dst unsafe.Pointer)
