#ifndef ESPRESSO_SERIALIZERS_H
#define ESPRESSO_SERIALIZERS_H

#include "Traits.h"

namespace Espresso
{
	template <typename StreamType>
	struct IOStreamBinarySerializer
	{
		StreamType& Stream;
	};

	template <typename StreamType>
	IOStreamBinarySerializer(StreamType&) -> IOStreamBinarySerializer<StreamType>;

	template <typename T>
	concept ArithmeticOrEnum = std::is_arithmetic_v<T> || std::is_enum_v<T>;

	template <typename T>
	concept ArithmeticOrEnumArray =
	    std::is_array_v<T> && ArithmeticOrEnum<std::remove_all_extents_t<T>>;

	template <typename StreamType, typename ArithmeticOrEnumOrArrayPrimitive>
	    requires ArithmeticOrEnum<ArithmeticOrEnumOrArrayPrimitive> ||
	             ArithmeticOrEnumArray<ArithmeticOrEnumOrArrayPrimitive>
	struct PrimitiveSerializerTraits<IOStreamBinarySerializer<StreamType>,
	                                 ArithmeticOrEnumOrArrayPrimitive>
	{
		static void Serialize(IOStreamBinarySerializer<StreamType>& serializer,
		                      ArithmeticOrEnumOrArrayPrimitive const& object)
		{
			serializer.Stream.write(reinterpret_cast<const char*>(std::addressof(object)),
			                        sizeof(object));
		}
	};

	template <typename StreamType, typename ArithmeticOrEnumOrArrayPrimitive>
	    requires ArithmeticOrEnum<ArithmeticOrEnumOrArrayPrimitive> ||
	             ArithmeticOrEnumArray<ArithmeticOrEnumOrArrayPrimitive>
	struct PrimitiveAssignableDeserializerTraits<IOStreamBinarySerializer<StreamType>,
	                                             ArithmeticOrEnumOrArrayPrimitive>
	{
		static void DeserializeAssign(IOStreamBinarySerializer<StreamType>& deserializer,
		                              ArithmeticOrEnumOrArrayPrimitive& object)
		{
			deserializer.Stream.read(reinterpret_cast<char*>(std::addressof(object)),
			                         sizeof(object));
		}
	};

	template <typename StreamType, typename ArithmeticOrEnumOrArrayPrimitive>
	    requires ArithmeticOrEnum<ArithmeticOrEnumOrArrayPrimitive> ||
	             ArithmeticOrEnumArray<ArithmeticOrEnumOrArrayPrimitive>
	struct PrimitiveConstructibleDeserializerTraits<IOStreamBinarySerializer<StreamType>,
	                                                ArithmeticOrEnumOrArrayPrimitive>
	{
		static auto DeserializeConstruct(IOStreamBinarySerializer<StreamType>& deserializer)
		{
			std::conditional_t<std::is_array_v<ArithmeticOrEnumOrArrayPrimitive>,
			                   MakeStdArray<ArithmeticOrEnumOrArrayPrimitive>,
			                   ArithmeticOrEnumOrArrayPrimitive>
			    obj;
			PrimitiveAssignableDeserializerTraits<
			    IOStreamBinarySerializer<StreamType>,
			    ArithmeticOrEnumOrArrayPrimitive>::DeserializeAssign(deserializer, obj);
			return obj;
		}
	};
} // namespace Espresso

#endif
