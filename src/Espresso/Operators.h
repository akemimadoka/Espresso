#ifndef ESPRESSO_OPERATORS_H
#define ESPRESSO_OPERATORS_H

#include "Traits.h"

namespace Espresso
{
	inline namespace Operators
	{
		template <typename Serializer, SerializableType<Serializer> T>
		constexpr Serializer& operator<<(Serializer& serializer, T const& obj)
		{
			SerializableTypeTraits<T, Serializer>::SerializeTo(obj, serializer);
			return serializer;
		}

		template <typename Deserializer, DeserializeAssignableType<Deserializer> T>
		constexpr Deserializer& operator>>(Deserializer& deserializer, T& obj)
		{
			DeserializeAssignableTypeTraits<T, Deserializer>::DeserializeAssignFrom(obj,
			                                                                        deserializer);
			return deserializer;
		}
	} // namespace Operators
} // namespace Espresso

#endif
