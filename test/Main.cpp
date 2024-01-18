#include "Espresso.h"

#include <fstream>
#include <cassert>
#include <iostream>

using namespace Espresso;

int main()
{
	Object<Field<"Field1", int>> testObject{ .Contents{ { 1 } } };
	constexpr char SerializedDataFileName[] = "SerializedData.bin";
	{
		std::ofstream fout(SerializedDataFileName, std::ios_base::binary);
		IOStreamBinarySerializer serializer{ fout };
		serializer << testObject;
	}
	{
		std::ifstream fin(SerializedDataFileName, std::ios_base::binary);
		IOStreamBinarySerializer deserializer{ fin };
		Object<Field<"Field1", int>> testObject2;
		deserializer >> testObject2;

		assert(testObject.GetField<"Field1">().Content == testObject2.GetField<"Field1">().Content);
	}

	FixedUInt<32> u1{ 12345 };
	FixedSInt<65> i1{ static_cast<FixedSInt<65>>(u1) };

	const auto result = u1 <=> i1;
	assert(result == std::strong_ordering::equivalent);

	{
		std::ofstream fout(SerializedDataFileName, std::ios_base::binary);
		IOStreamBinarySerializer serializer{ fout };
		serializer << u1 << i1;
	}
	{
		std::ifstream fin(SerializedDataFileName, std::ios_base::binary);
		IOStreamBinarySerializer deserializer{ fin };

		FixedUInt<32> u2{};
		FixedSInt<65> i2{};
		deserializer >> u2 >> i2;

		assert(u1 == u2 && i1 == i2);
	}

	ComposedInt<std::endian::little, FixedUInt<62>, FixedSInt<5>, FixedUInt<61>> composed{};
	composed.Set<0>(FixedUInt<62>(1145141919810));
	composed.Set<1>(FixedSInt<5>(0b11010));
	composed.Set<2>(FixedUInt<61>(1919810114514));
	assert(composed.Get<0>() == FixedUInt<62>(1145141919810));
	assert(composed.Get<1>() == FixedSInt<5>(0b11010));
	assert(composed.Get<2>() == FixedUInt<61>(1919810114514));

	composed.Set<1>(FixedSInt<5>(0b10011));
	assert(composed.Get<0>() == FixedUInt<62>(1145141919810));
	assert(composed.Get<1>() == FixedSInt<5>(0b10011));
	assert(composed.Get<2>() == FixedUInt<61>(1919810114514));
}
