#ifndef ESPRESSO_TRAITS_H
#define ESPRESSO_TRAITS_H

#include <type_traits>
#include <array>

namespace Espresso
{
	namespace Detail
	{
		template <std::size_t Index, typename TypeSequence, template <typename> class Predicate,
		          std::size_t Fallback>
		struct FindFirstIndexOfImpl
		{
			static constexpr std::size_t value = Fallback;
		};

		template <std::size_t Index, template <typename...> class TypeSequenceTemplate,
		          typename TFirst, typename... TRest, template <typename> class Predicate,
		          std::size_t Fallback>
		struct FindFirstIndexOfImpl<Index, TypeSequenceTemplate<TFirst, TRest...>, Predicate,
		                            Fallback>
		    : std::conditional_t<Predicate<TFirst>::value,
		                         std::integral_constant<std::size_t, Index>,
		                         FindFirstIndexOfImpl<Index + 1, TypeSequenceTemplate<TRest...>,
		                                              Predicate, Fallback>>
		{
		};

		template <typename TypeSequence, template <typename> class Predicate, typename Fallback>
		struct FindFirstOfImpl : std::type_identity<Fallback>
		{
		};

		template <template <typename...> class TypeSequenceTemplate, typename TFirst,
		          typename... TRest, template <typename> class Predicate, typename Fallback>
		struct FindFirstOfImpl<TypeSequenceTemplate<TFirst, TRest...>, Predicate, Fallback>
		    : std::conditional_t<
		          Predicate<TFirst>::value, std::type_identity<TFirst>,
		          FindFirstOfImpl<TypeSequenceTemplate<TRest...>, Predicate, Fallback>>
		{
		};

		template <typename TypeSequence, typename... TRest>
		struct ReverseImpl;

		template <template <typename...> class TypeSequenceTemplate, typename... T>
		struct ReverseImpl<TypeSequenceTemplate<T...>>
		    : std::type_identity<TypeSequenceTemplate<T...>>
		{
		};

		template <template <typename...> class TypeSequenceTemplate, typename... T, typename TFirst,
		          typename... TRest>
		struct ReverseImpl<TypeSequenceTemplate<T...>, TFirst, TRest...>
		    : ReverseImpl<TypeSequenceTemplate<TFirst, T...>, TRest...>
		{
		};

		template <typename TypeSequence, template <typename...> class Func, typename Result,
		          typename... Prefixes>
		struct ForAllPrefixImpl : std::type_identity<Result>
		{
		};

		template <template <typename...> class TypeSequenceTemplate, typename TFirst,
		          typename... TRest, template <typename...> class Func,
		          template <typename...> class ResultContainer, typename... Results,
		          typename... Prefixes>
		struct ForAllPrefixImpl<TypeSequenceTemplate<TFirst, TRest...>, Func,
		                        ResultContainer<Results...>, Prefixes...>
		    : ForAllPrefixImpl<
		          TypeSequenceTemplate<TRest...>, Func,
		          ResultContainer<Results..., typename Func<Prefixes..., TFirst>::type>,
		          Prefixes..., TFirst>
		{
		};
	} // namespace Detail

	template <typename TypeSequence>
	struct FirstOf;

	template <template <typename...> class TypeSequenceTemplate, typename TFirst, typename... TRest>
	struct FirstOf<TypeSequenceTemplate<TFirst, TRest...>> : std::type_identity<TFirst>
	{
	};

	template <typename TypeSequence, template <typename> class Predicate,
	          std::size_t Fallback = std::size_t(-1)>
	struct FindFirstIndexOf : Detail::FindFirstIndexOfImpl<0, TypeSequence, Predicate, Fallback>
	{
	};

	template <typename TypeSequence>
	struct Reverse;

	template <template <typename...> class TypeSequenceTemplate, typename... T>
	struct Reverse<TypeSequenceTemplate<T...>> : Detail::ReverseImpl<TypeSequenceTemplate<>, T...>
	{
	};

	template <typename TypeSequence, template <typename> class Predicate, typename Fallback = void>
	struct FindFirstOf : Detail::FindFirstOfImpl<TypeSequence, Predicate, Fallback>
	{
	};

	template <typename TypeSequence, template <typename...> class Func,
	          template <typename...> class ResultContainer>
	struct ForAllPrefix : Detail::ForAllPrefixImpl<TypeSequence, Func, ResultContainer<>>
	{
	};

	template <typename TypeSequence, template <typename> class Func>
	struct TypeSequenceToArray;

	template <template <typename...> class TypeSequenceTemplate, typename... T,
	          template <typename> class Func>
	struct TypeSequenceToArray<TypeSequenceTemplate<T...>, Func>
	{
		static constexpr std::array value{ Func<T>::value... };
	};

	template <typename FromType, template <typename...> class ToTemplate>
	struct ApplyTo;

	template <template <typename...> class FromTemplate, typename... T,
	          template <typename...> class ToTemplate>
	struct ApplyTo<FromTemplate<T...>, ToTemplate> : std::type_identity<ToTemplate<T...>>
	{
	};

	template <typename T, typename Primitive>
	struct PrimitiveSerializerTraits;
	// static constexpr void Serialize(T& serializer, Primitive const& object);

	template <typename Serializer, typename Primitive>
	concept PrimitiveSerializer = requires(Serializer& serializer, Primitive const& primitive) {
		{
			PrimitiveSerializerTraits<Serializer, Primitive>::Serialize(serializer, primitive)
		};
	};

	template <typename Serializer, typename... Primitives>
	concept MultiplePrimitiveSerializer = (... && PrimitiveSerializer<Serializer, Primitives>);

	template <typename T, typename Primitive>
	struct PrimitiveConstructibleDeserializerTraits;
	// static constexpr Primitive DeserializeConstruct(T& deserializer);

	template <typename T, typename Primitive>
	struct PrimitiveAssignableDeserializerTraits;
	// static constexpr void DeserializeAssign(T& deserializer, Primitive& object);

	template <typename Deserializer, typename Primitive>
	concept PrimitiveConstructibleDeserializer = requires(Deserializer& deserializer) {
		{
			PrimitiveConstructibleDeserializerTraits<Deserializer, Primitive>::DeserializeConstruct(
			    deserializer)
		} -> std::same_as<Primitive>;
	};

	template <typename Deserializer, typename... Primitives>
	concept MultiplePrimitiveConstructibleDeserializer =
	    (... && PrimitiveConstructibleDeserializer<Deserializer, Primitives>);

	template <typename Deserializer, typename Primitive>
	concept PrimitiveAssignableDeserializer =
	    requires(Deserializer& deserializer, Primitive& primitive) {
		    {
			    PrimitiveAssignableDeserializerTraits<Deserializer, Primitive>::DeserializeAssign(
			        deserializer, primitive)
		    };
	    };

	template <typename Deserializer, typename... Primitives>
	concept MultiplePrimitiveAssignableDeserializer =
	    (... && PrimitiveAssignableDeserializer<Deserializer, Primitives>);

	template <typename Deserializer, typename Primitive>
	concept PrimitiveDeserializer = PrimitiveConstructibleDeserializer<Deserializer, Primitive> &&
	                                PrimitiveAssignableDeserializer<Deserializer, Primitive>;

	template <typename Deserializer, typename... Primitives>
	concept MultiplePrimitiveDeserializer =
	    MultiplePrimitiveConstructibleDeserializer<Deserializer, Primitives...> &&
	    MultiplePrimitiveAssignableDeserializer<Deserializer, Primitives...>;

	template <typename T, typename Serializer>
	struct SerializableTypeTraits;
	// static constexpr void SerializeTo(T const& obj, Serializer& serializer);

	// 为 PrimitiveSerializer 自动实现 Primitive 的 SerializableTypeTraits
	template <typename T, PrimitiveSerializer<T> Serializer>
	struct SerializableTypeTraits<T, Serializer>
	{
		static constexpr void SerializeTo(T const& obj, Serializer& serializer)
		{
			PrimitiveSerializerTraits<Serializer, T>::Serialize(serializer, obj);
		}
	};

	template <typename T, typename Serializer>
	concept SerializableType = requires(T const& obj, Serializer& serializer) {
		{
			SerializableTypeTraits<T, Serializer>::SerializeTo(obj, serializer)
		};
	};

	template <typename T, typename Deserializer>
	struct DeserializeConstructibleTypeTraits;
	// static constexpr T DeserializeConstructFrom(Deserializer& deserializer);

	template <typename T, typename Deserializer>
	struct DeserializeAssignableTypeTraits;
	// static constexpr void DeserializeAssignFrom(T& obj, Deserializer& deserializer);

	// 为 Primitive(Constructible/Assignable)Deserializer 自动实现 Primitive 的 Deserialize(Constructible/Assignable)TypeTraits
	template <typename T, PrimitiveConstructibleDeserializer<T> Deserializer>
	struct DeserializeConstructibleTypeTraits<T, Deserializer>
	{
		static constexpr T DeserializeConstructFrom(Deserializer& deserializer)
		{
			return PrimitiveConstructibleDeserializerTraits<Deserializer, T>::DeserializeConstruct(
			    deserializer);
		}
	};

	template <typename T, PrimitiveAssignableDeserializer<T> Deserializer>
	struct DeserializeAssignableTypeTraits<T, Deserializer>
	{
		static constexpr void DeserializeAssignFrom(T& obj, Deserializer& deserializer)
		{
			return PrimitiveAssignableDeserializerTraits<Deserializer, T>::DeserializeAssign(
			    deserializer, obj);
		}
	};

	template <typename T, typename Deserializer>
	concept DeserializeConstructibleType = requires(Deserializer& deserializer) {
		{
			DeserializeConstructibleTypeTraits<T, Deserializer>::DeserializeConstructFrom(
			    deserializer)
		} -> std::same_as<T>;
	};

	template <typename T, typename Deserializer>
	concept DeserializeAssignableType = requires(T& obj, Deserializer& deserializer) {
		{
			DeserializeAssignableTypeTraits<T, Deserializer>::DeserializeAssignFrom(obj,
			                                                                        deserializer)
		};
	};

	template <typename T, typename Deserializer>
	concept DeserializableType =
	    DeserializeConstructibleType<T, Deserializer> && DeserializeAssignableType<T, Deserializer>;

	// 可平凡初始化的类型若实现了 DeserializeAssignableType，将自动实现 DeserializeConstructibleType，从而自动成为 DeserializableType
	// 也可以通过实现 AutoImplementDeserializeConstructibleType，提供默认初始化器来快速为自己的类型实现 DeserializeConstructibleType
	// 通过 DeserializeAssignableType 实现 DeserializeConstructibleType 而非反向的原因是 DeserializeConstructibleType 总是需要构造新的类型，若用于实现 DeserializeAssignableType 可能造成非必要的对象构造
	template <typename T>
	struct AutoImplementDeserializeConstructibleTypeTraits;
	// static constexpr T Initializer();

	template <typename T>
	    requires std::is_trivially_constructible_v<T>
	struct AutoImplementDeserializeConstructibleTypeTraits<T>
	{
		static constexpr T Initializer()
		{
			return T();
		}
	};

	template <typename T>
	concept AutoImplementDeserializeConstructibleType = requires {
		{
			AutoImplementDeserializeConstructibleTypeTraits<T>::Initializer()
		} -> std::same_as<T>;
	};

	template <typename T, typename Deserializer>
	    requires AutoImplementDeserializeConstructibleType<T> &&
	             DeserializeAssignableType<T, Deserializer>
	struct DeserializeConstructibleTypeTraits<T, Deserializer>
	{
		static constexpr T DeserializeConstructFrom(Deserializer& deserializer)
		{
			T obj(AutoImplementDeserializeConstructibleTypeTraits<T>::Initializer());
			DeserializeAssignableTypeTraits<T, Deserializer>::DeserializeAssignFrom(obj,
			                                                                        deserializer);
			return obj;
		}
	};
} // namespace Espresso

#endif
