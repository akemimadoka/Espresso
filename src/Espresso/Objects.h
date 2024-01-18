#ifndef ESPRESSO_OBJECTS_H
#define ESPRESSO_OBJECTS_H

#include "Misc.h"
#include "Traits.h"

#include <span>
#include <cstring>
#include <ranges>
#include <bit>
#include <limits>
#include <stdexcept>
#include <optional>

namespace Espresso
{
	template <typename T>
	struct DynamicSizedTypeTraits;
	// static constexpr std::size_t GetSize(const T& obj);

	template <typename T>
	concept DynamicSizedType = requires(const T& obj) {
		{
			DynamicSizedTypeTraits<T>::GetSize(obj)
		} -> std::same_as<std::size_t>;
	};

	struct FallbackDummy
	{
	};

	inline constexpr std::size_t EffectiveIntBitSize = sizeof(std::uintptr_t) * CHAR_BIT;

	template <std::size_t BitSize, typename FallbackType, std::integral... TInt>
	struct FindFirstIntegerOrArrayFits
	{
		template <std::size_t TargetBitSize>
		struct PerfectlyFitsInBitsBinder
		{
			template <std::integral T>
			struct Traits : std::bool_constant<TargetBitSize == sizeof(T) * CHAR_BIT>
			{
			};
		};

		template <std::integral T>
		struct PerfectlyFitsInBitsOfArray
		    : std::bool_constant<BitSize % (sizeof(T) * CHAR_BIT) == 0>
		{
		};

		template <std::integral T>
		struct FitsInBits : std::bool_constant<BitSize <= sizeof(T) * CHAR_BIT>
		{
		};

		using PerfectlyFitsInType =
		    typename FindFirstOf<std::tuple<TInt...>,
		                         PerfectlyFitsInBitsBinder<BitSize>::template Traits,
		                         FallbackType>::type;
		using PerfectlyFitsInArrayElementType =
		    typename FindFirstOf<typename Reverse<std::tuple<TInt...>>::type,
		                         PerfectlyFitsInBitsOfArray, FallbackType>::type;
		using FitsInType =
		    typename FindFirstOf<std::tuple<TInt...>, FitsInBits, FallbackType>::type;
		using PerfectlyFitsInEffectiveIntType =
		    typename FindFirstOf<std::tuple<TInt...>,
		                         PerfectlyFitsInBitsBinder<EffectiveIntBitSize>::template Traits,
		                         FallbackType>::type;
	};

	// TODO: 为 DynamicInt 预留
	// template <typename T>
	// struct IntTraits
	// {
	// 	static constexpr std::size_t GetBitSize(T const&);
	// 	static constexpr bool IsSigned(T const&);
	// 	template <typename U>
	// 	static constexpr std::optional<U> Cast(T const&);
	// };

	// UnderlyingInts 必须按从小到大排序，必须具有相同的符号
	template <std::size_t BitSize, std::endian Endian, std::integral... UnderlyingInts>
	struct FixedInt
	{
		// 端序可能既不是小端又不是大端（如混合端序），暂时不支持此类端序
		static_assert(Endian == std::endian::little || Endian == std::endian::big);

	public:
		using EffectiveType =
		    FixedInt<(BitSize + EffectiveIntBitSize) / EffectiveIntBitSize * EffectiveIntBitSize,
		             Endian, UnderlyingInts...>;

		// 若单个 UnderlyingInts 中存在整数类型可以准确地容纳 BitSize 的整数，则为符合条件中的最小的整数类型
		// 否则，若以上列表中存在整数类型可以以数组形式准确地容纳 BitSize 的整数，则为符合条件中的最大的整数类型的数组
		// 否则，若以上列表中存在整数类型足以容纳 BitSize 的整数，则为符合条件中的最小的整数类型
		// 否则，若 UnderlyingInts 至少存在一个位宽与 EffectiveIntBitSize 相同的类型，则为第一个出现的符合条件的类型或其数组
		// 否则，为 UnderlyingInts 中第一个出现的类型的数组
		// 按数组序列化时，按照最小消费位数进行（例如 65 BitSize 时，可能消耗 5 个 int8 而非 2 个 int64）
		// 是数组的情况下，数组的元素顺序也按照 endian 存储
		// 多余的位总是在高位的更高方向作为填充位，若值为负数，则全为 1，否则全为 0
		// 如果以上行为不是用户需要的行为，则需要手动对齐到字节边界，或者手写新的类型
		using ValueType = decltype([] {
			using Traits = FindFirstIntegerOrArrayFits<BitSize, FallbackDummy, UnderlyingInts...>;
			if constexpr (!std::is_same_v<typename Traits::PerfectlyFitsInType, FallbackDummy>)
			{
				return std::type_identity<typename Traits::PerfectlyFitsInType>{};
			}
			else if constexpr (!std::is_same_v<typename Traits::PerfectlyFitsInArrayElementType,
			                                   FallbackDummy>)
			{
				return std::type_identity<
				    typename Traits::PerfectlyFitsInArrayElementType
				        [BitSize / CHAR_BIT /
				         sizeof(typename Traits::PerfectlyFitsInArrayElementType)]>{};
			}
			else if constexpr (!std::is_same_v<typename Traits::FitsInType, FallbackDummy>)
			{
				return std::type_identity<typename Traits::FitsInType>{};
			}
			else if constexpr (!std::is_same_v<typename Traits::PerfectlyFitsInEffectiveIntType,
			                                   FallbackDummy>)
			{
				using Type = typename Traits::PerfectlyFitsInEffectiveIntType;
				constexpr auto TypeBitSize = sizeof(Type) * CHAR_BIT;
				if constexpr (TypeBitSize >= BitSize)
				{
					return std::type_identity<Type>{};
				}
				else
				{
					return std::type_identity<Type[(BitSize + TypeBitSize) / TypeBitSize]>{};
				}
			}
			else
			{
				using SmallestInt = typename FirstOf<std::tuple<UnderlyingInts...>>::type;
				return std::type_identity<
				    SmallestInt[(BitSize + CHAR_BIT) / CHAR_BIT / sizeof(SmallestInt)]>{};
			}
		}())::type;
		using ElemType = std::remove_all_extents_t<ValueType>;
		ValueType Value;

		static constexpr FixedInt FromIntegral(std::integral auto value) noexcept
		{
			return FixedInt(
			    FixedInt<sizeof(value) * CHAR_BIT, std::endian::native, UnderlyingInts...>(value));
		}

		static constexpr bool IsSigned = std::is_signed_v<ElemType>;

		constexpr bool IsNegative() const noexcept
		{
			if constexpr (IsSigned)
			{
				if constexpr (std::is_array_v<ValueType>)
				{
					return Value[std::size(Value) - 1] < 0;
				}
				else
				{
					return Value < 0;
				}
			}
			else
			{
				return false;
			}
		}

		constexpr auto ByteSwap() const noexcept
		{
			FixedInt<BitSize,
			         Endian == std::endian::little ? std::endian::big : std::endian::little,
			         UnderlyingInts...>
			    copy{};
			constexpr auto UnalignedBits = BitSize % (sizeof(ElemType) * CHAR_BIT);
			if constexpr (std::is_array_v<ValueType>)
			{
				for (std::size_t i = 0; i < std::extent_v<ValueType>; ++i)
				{
					copy.Value[std::extent_v<ValueType> - i - 1] = std::byteswap(Value[i]);
				}
			}
			else
			{
				copy.Value = std::byteswap(Value);
			}

			return copy;
		}

		template <std::size_t ToBitSize, std::endian ToEndian, std::integral... ToUnderlyingInts>
		constexpr explicit
		operator FixedInt<ToBitSize, ToEndian, ToUnderlyingInts...>() const noexcept
		{
			const auto sourceSpan = [&] {
				if constexpr (std::is_array_v<ValueType>)
				{
					return std::span(Value);
				}
				else
				{
					return std::span(std::addressof(Value), 1);
				}
			}();
			using SourceElemType = decltype(sourceSpan)::element_type;
			constexpr auto SourceElemBitSize = sizeof(SourceElemType) * CHAR_BIT;

			using ResultType = FixedInt<ToBitSize, ToEndian, ToUnderlyingInts...>;
			ResultType result{};
			if (IsNegative())
			{
				if constexpr (std::is_array_v<typename ResultType::ValueType>)
				{
					std::fill(std::begin(result.Value), std::end(result.Value),
					          ~(std::remove_all_extents_t<typename ResultType::ValueType>{}));
				}
				else
				{
					result.Value = ~(typename ResultType::ValueType{});
				}
			}
			const auto resultSpan = [&] {
				if constexpr (std::is_array_v<typename ResultType::ValueType>)
				{
					return std::span(result.Value);
				}
				else
				{
					return std::span(std::addressof(result.Value), 1);
				}
			}();

			using ResultElemType = decltype(resultSpan)::element_type;
			constexpr auto ResultElemBitSize = sizeof(ResultElemType) * CHAR_BIT;

			static_assert(SourceElemBitSize % ResultElemBitSize == 0 ||
			              ResultElemBitSize % SourceElemBitSize == 0);

			// 无论端序，从低位到高位处理
			// 小端从低位到高位存储，大端相反
			// 填充位需在判断符号位后设置
			// 计算复制消耗的来源和结果数量
			constexpr auto ProcessBitSize = std::min(BitSize, ToBitSize);
			// 向上取整
			constexpr auto SourceCount =
			    (ProcessBitSize + SourceElemBitSize - 1) / SourceElemBitSize;
			constexpr auto ResultCount =
			    (ProcessBitSize + ResultElemBitSize - 1) / ResultElemBitSize;

			const auto iteratorCreator =
			    [&]<std::endian RangeEndian>(std::integral_constant<std::endian, RangeEndian>,
			                                 auto span, std::size_t count) {
				    if constexpr (Endian == std::endian::little)
				    {
					    return std::ranges::subrange(span.begin(),
					                                 std::prev(span.end(), span.size() - count));
				    }
				    else
				    {
					    return std::ranges::subrange(span.rbegin(),
					                                 std::prev(span.rend(), span.size() - count));
				    }
			    };
			auto [resultCur, resultEnd] = iteratorCreator(
			    std::integral_constant<std::endian, ToEndian>{}, resultSpan, ResultCount);
			std::size_t singleResultElemFilledBits = 0;
			for (auto sourceElem : iteratorCreator(std::integral_constant<std::endian, Endian>{},
			                                       sourceSpan, SourceCount))
			{
				if constexpr (Endian != ToEndian)
				{
					sourceElem = std::byteswap(sourceElem);
				}

				if constexpr (SourceElemBitSize < ResultElemBitSize)
				{
					*resultCur |=
					    (static_cast<ResultElemType>(sourceElem) << singleResultElemFilledBits);
					singleResultElemFilledBits += SourceElemBitSize;
					if (singleResultElemFilledBits == ResultElemBitSize)
					{
						++resultCur;
						singleResultElemFilledBits = 0;
					}
				}
				else if constexpr (SourceElemBitSize == ResultElemBitSize)
				{
					*resultCur = static_cast<ResultElemType>(sourceElem);
					++resultCur;
					if (resultCur == resultEnd)
					{
						break;
					}
				}
				else
				{
					const auto totalFillCount =
					    std::min(SourceElemBitSize / ResultElemBitSize, resultSpan.size());
					for (std::size_t filledResultElemCount = 0;
					     filledResultElemCount < totalFillCount; ++filledResultElemCount)
					{
						// C++20 之后，整数类型必定使用 2 的补码实现，因此窄转换必定结果是截断，不需要多余处理
						*resultCur++ = static_cast<ResultElemType>(
						    sourceElem >> (filledResultElemCount * ResultElemBitSize));
					}
				}
			}

			return result;
		}

		template <std::integral ToType>
		constexpr explicit operator ToType() const noexcept
		{
			return static_cast<FixedInt<sizeof(ToType) * CHAR_BIT, std::endian::native, ToType>>(
			           *this)
			    .Value;
		}

		template <typename T>
		constexpr T Cast() const noexcept
		    requires requires {
			    {
				    static_cast<T>(*this)
			    };
		    }
		{
			return static_cast<T>(*this);
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr std::strong_ordering operator<=>(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			using OtherInt = FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...>;

			if constexpr (Endian == OtherEndian)
			{
				if constexpr (IsSigned == OtherInt::IsSigned)
				{
					if constexpr (BitSize == OtherBitSize)
					{
						if constexpr (std::is_array_v<ValueType>)
						{
							// 从高位开始比较
							if constexpr (Endian == std::endian::little)
							{
								for (std::size_t i = std::size(Value); i-- > 0;)
								{
									if (const auto result = Value[i] <=> other.Value[i];
									    result != std::strong_ordering::equivalent)
									{
										return result;
									}
								}
								return std::strong_ordering::equivalent;
							}
							else
							{
								for (std::size_t i = 0; i < std::size(Value); ++i)
								{
									if (const auto result = Value[i] <=> other.Value[i];
									    result != std::strong_ordering::equivalent)
									{
										return result;
									}
								}
								return std::strong_ordering::equivalent;
							}
						}
						else
						{
							return Value <=> other.Value;
						}
					}
					else if constexpr (BitSize > OtherBitSize)
					{
						return *this <=>
						       static_cast<FixedInt<BitSize, OtherEndian, OtherUnderlyingInts...>>(
						           other);
					}
					else
					{
						return static_cast<FixedInt<OtherBitSize, Endian, UnderlyingInts...>>(
						           *this) <=> other;
					}
				}
				else
				{
					if constexpr (IsSigned)
					{
						// 无符号值域超出相同 BitSize 的有符号值域，因此转换到至少多 1 的有符号值再比较
						return *this <=>
						       static_cast<FixedInt<
						           (BitSize > OtherBitSize ? BitSize : OtherBitSize + 1),
						           OtherEndian, std::make_signed_t<OtherUnderlyingInts>...>>(other);
					}
					else
					{
						return static_cast<
						           FixedInt<(BitSize > OtherBitSize ? BitSize : BitSize + 1),
						                    Endian, std::make_signed_t<UnderlyingInts>...>>(
						           *this) <=> other;
					}
				}
			}
			else
			{
				return ByteSwap() <=> other;
			}
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr bool operator==(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return is_eq(*this <=> other);
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts, typename Continuation>
		constexpr auto UsualArithmeticConversion(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other,
		    Continuation&& contination) const noexcept
		{
			using OtherType = FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...>;

			if constexpr (Endian == OtherEndian)
			{
				constexpr auto NearestEffectiveIntBitSize =
				    (std::max(BitSize, OtherBitSize) + EffectiveIntBitSize - 1) /
				    EffectiveIntBitSize * EffectiveIntBitSize;

				if constexpr (IsSigned == OtherType::IsSigned && BitSize == OtherBitSize &&
				              BitSize == NearestEffectiveIntBitSize)
				{
					return std::forward<Continuation>(contination)(*this, other);
				}
				else
				{
					if constexpr (BitSize == OtherBitSize && BitSize == NearestEffectiveIntBitSize)
					{
						// 已讨论相同符号的问题
						// 提升后若 U BitSize >= S BitSize，则选择 U
						// 否则，若 S ⊇ U，则选择 S
						// 否则，选择 make_unsigned_t<S>
						if constexpr (IsSigned)
						{
							return static_cast<FixedInt<BitSize, Endian,
							                            std::make_unsigned_t<UnderlyingInts>...>>(
							           *this)
							    .UsualArithmeticConversion(other,
							                               std::forward<Continuation>(contination));
						}
						else
						{
							return UsualArithmeticConversion(
							    static_cast<FixedInt<OtherBitSize, OtherEndian,
							                         std::make_unsigned_t<OtherUnderlyingInts>...>>(
							        other),
							    std::forward<Continuation>(contination));
						}
					}
					else if constexpr (BitSize == NearestEffectiveIntBitSize)
					{
						if constexpr (IsSigned == OtherType::IsSigned || IsSigned)
						{
							// 相同符号下，A > B 的情况，或者不同符号下 S ⊇ U 的情况
							return UsualArithmeticConversion(
							    static_cast<FixedInt<NearestEffectiveIntBitSize, OtherEndian,
							                         std::make_signed_t<UnderlyingInts>...>>(other),
							    std::forward<Continuation>(contination));
						}
						else
						{
							// make_unsigned_t<S> 的情况
							return UsualArithmeticConversion(
							    static_cast<FixedInt<NearestEffectiveIntBitSize, OtherEndian,
							                         std::make_unsigned_t<OtherUnderlyingInts>...>>(
							        other),
							    std::forward<Continuation>(contination));
						}
					}
					else if constexpr (OtherBitSize == NearestEffectiveIntBitSize)
					{
						if constexpr (IsSigned == OtherType::IsSigned || OtherType::IsSigned)
						{
							// 相同符号下，A > B 的情况，或者不同符号下 S ⊇ U 的情况
							return static_cast<FixedInt<NearestEffectiveIntBitSize, Endian,
							                            std::make_signed_t<UnderlyingInts>...>>(
							           *this)
							    .UsualArithmeticConversion(other,
							                               std::forward<Continuation>(contination));
						}
						else
						{
							// make_unsigned_t<S> 的情况
							return static_cast<FixedInt<NearestEffectiveIntBitSize, Endian,
							                            std::make_unsigned_t<UnderlyingInts>...>>(
							           *this)
							    .UsualArithmeticConversion(other,
							                               std::forward<Continuation>(contination));
						}
					}
					else
					{
						// 未提升，双方提升后再进行
						return static_cast<
						           FixedInt<NearestEffectiveIntBitSize, Endian, UnderlyingInts...>>(
						           *this)
						    .UsualArithmeticConversion(
						        static_cast<FixedInt<NearestEffectiveIntBitSize, OtherEndian,
						                             OtherUnderlyingInts...>>(other),
						        std::forward<Continuation>(contination));
					}
				}
			}
			else
			{
				if constexpr (Endian == std::endian::native)
				{
					return UsualArithmeticConversion(
					    static_cast<
					        FixedInt<OtherBitSize, std::endian::native, OtherUnderlyingInts...>>(
					        other),
					    std::forward<Continuation>(contination));
				}
				else
				{
					return static_cast<FixedInt<BitSize, std::endian::native, UnderlyingInts...>>(
					           *this)
					    .UsualArithmeticConversion(other, std::forward<Continuation>(contination));
				}
			}
		}

		enum class IterateOrder
		{
			LowToHigh,
			HighToLow,
		};

		template <IterateOrder Order, std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts, typename Continuation>
		constexpr auto
		BinaryOperation(FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other,
		                Continuation&& contination) const noexcept
		{
			return UsualArithmeticConversion(
			    other, [&]<std::size_t CommonBitSize, std::endian CommonEndian,
			               std::integral... AUnderlyingInts, std::integral... BUnderlyingInts>(
			               FixedInt<CommonBitSize, CommonEndian, AUnderlyingInts...> const& a,
			               FixedInt<CommonBitSize, CommonEndian, BUnderlyingInts...> const& b) {
				    FixedInt<CommonBitSize, CommonEndian, AUnderlyingInts...> result;
				    if constexpr (std::is_array_v<typename FixedInt<CommonBitSize, CommonEndian,
				                                                    AUnderlyingInts...>::ValueType>)
				    {
					    // 此时 a b 的 endian 应当相同
					    for (const auto i : a.template ForEachElementIndexesRange<Order>())
					    {
						    std::forward<Continuation>(contination)(result.Value[i], a.Value[i],
						                                            b.Value[i]);
					    }
				    }
				    else
				    {
					    std::forward<Continuation>(contination)(result.Value, a.Value, b.Value);
				    }
				    return result;
			    });
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr auto operator+(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return BinaryOperation<IterateOrder::LowToHigh>(
			    other, [carried = false](auto& result, auto a, auto b) mutable {
				    using ElemType = decltype(a);
				    using UnsignedElemType = std::make_unsigned_t<ElemType>;

				    // 除最高位所在元素以外，其他元素全部视为无符号位
				    const auto aValue = static_cast<UnsignedElemType>(a);
				    const auto bValue = static_cast<UnsignedElemType>(b);
				    const auto sumValue = aValue + bValue;
				    // 若未溢出，则两值之和不小于其中任意一个值，若溢出，则两值之和小于其中任意一个值
				    const auto currentCarried = sumValue < aValue;
				    const auto addWithCarry = static_cast<ElemType>(sumValue + carried);
				    result = addWithCarry;
				    // carried 仅能转换到 0/1，因此若溢出，addWithCarry 必定为 0
				    carried = currentCarried || addWithCarry == 0;
			    });
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr auto operator-(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return BinaryOperation<IterateOrder::HighToLow>(
			    other, [borrowed = false](auto& result, auto a, auto b) mutable {
				    using ElemType = decltype(a);
				    using UnsignedElemType = std::make_unsigned_t<ElemType>;

				    const auto aValue = static_cast<UnsignedElemType>(a);
				    const auto bValue = static_cast<UnsignedElemType>(b);
				    const auto substractValue = aValue - bValue;
				    // 若未溢出，则两值之差不小于被减数，若溢出，则两数之差大于被减数
				    const auto currentBorrowed = substractValue > aValue;
				    const auto substractWithBorrowed =
				        static_cast<ElemType>(substractValue - borrowed);
				    result = substractWithBorrowed;
				    // borrowed 仅能转换到 0/1，因此若溢出，substractWithBorrowed 必定为 static_cast<ElemType>(-1)
				    borrowed =
				        currentBorrowed || substractWithBorrowed == static_cast<ElemType>(-1);
			    });
		}

		constexpr FixedInt operator-() const noexcept
		{
			return FixedInt{} - *this;
		}

		template <IterateOrder Order>
		constexpr auto ForEachElementIndexesRange() const noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				constexpr bool LeftToRight =
				    (Endian == std::endian::little && Order == IterateOrder::LowToHigh) ||
				    (Endian == std::endian::big && Order == IterateOrder::HighToLow);
				if constexpr (LeftToRight)
				{
					return std::views::iota(std::size_t(0), std::size(Value));
				}
				else
				{
					return std::views::iota(std::size_t(0), std::size(Value)) | std::views::reverse;
				}
			}
			else
			{
				return std::views::single(std::size_t(0));
			}
		}

		template <IterateOrder Order>
		constexpr auto ForEachElementsRange() noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				constexpr bool LeftToRight =
				    (Endian == std::endian::little && Order == IterateOrder::LowToHigh) ||
				    (Endian == std::endian::big && Order == IterateOrder::HighToLow);
				if constexpr (LeftToRight)
				{
					return std::ranges::subrange(std::begin(Value), std::end(Value));
				}
				else
				{
					return std::ranges::subrange(std::rbegin(Value), std::rend(Value));
				}
			}
			else
			{
				return std::ranges::subrange(std::addressof(Value), std::addressof(Value) + 1);
			}
		}

		template <IterateOrder Order>
		constexpr auto ForEachElementsRange() const noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				constexpr bool LeftToRight =
				    (Endian == std::endian::little && Order == IterateOrder::LowToHigh) ||
				    (Endian == std::endian::big && Order == IterateOrder::HighToLow);
				if constexpr (LeftToRight)
				{
					return std::ranges::subrange(std::begin(Value), std::end(Value));
				}
				else
				{
					return std::ranges::subrange(std::rbegin(Value), std::rend(Value));
				}
			}
			else
			{
				return std::ranges::subrange(std::addressof(Value), std::addressof(Value) + 1);
			}
		}

		constexpr std::size_t CountLeadingZeroes() const noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				std::size_t result{};
				for (const auto element : ForEachElementsRange<IterateOrder::HighToLow>())
				{
					const auto leadingZeroes = std::countl_zero(element);
					result += leadingZeroes;
					if (leadingZero != sizeof(ElemType) * CHAR_BIT)
					{
						// 已发现第一个 1
						break;
					}
				}
				return result;
			}
			else
			{
				return std::countl_zero(Value);
			}
		}

		constexpr std::size_t CountLeadingOnes() const noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				std::size_t result{};
				for (const auto element : ForEachElementsRange<IterateOrder::HighToLow>())
				{
					const auto leadingOnes = std::countl_one(element);
					result += leadingOnes;
					if (leadingOnes != sizeof(ElemType) * CHAR_BIT)
					{
						// 已发现第一个 0
						break;
					}
				}
				return result;
			}
			else
			{
				return std::countl_one(Value);
			}
		}

		constexpr std::size_t CountTrailingZeroes() const noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				std::size_t result{};
				for (const auto element : ForEachElementsRange<IterateOrder::LowToHigh>())
				{
					const auto trailingZeroes = std::countr_zero(element);
					result += trailingZeroes;
					if (trailingZeroes != sizeof(ElemType) * CHAR_BIT)
					{
						// 已发现第一个 1
						break;
					}
				}
				return result;
			}
			else
			{
				return std::countr_zero(Value);
			}
		}

		constexpr std::size_t CountTrailingOnes() const noexcept
		{
			if constexpr (std::is_array_v<ValueType>)
			{
				std::size_t result{};
				for (const auto element : ForEachElementsRange<IterateOrder::LowToHigh>())
				{
					const auto trailingOnes = std::countr_one(element);
					result += trailingOnes;
					if (trailingOnes != sizeof(ElemType) * CHAR_BIT)
					{
						// 已发现第一个 0
						break;
					}
				}
				return result;
			}
			else
			{
				return std::countl_one(Value);
			}
		}

		constexpr FixedInt operator~() const noexcept
		{
			FixedInt result(*this);
			if constexpr (std::is_array_v<ValueType>)
			{
				for (auto& value : result.Value)
				{
					value = ~value;
				}
			}
			else
			{
				result.Value = ~result.Value;
			}
			return result;
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt operator<<(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			// 位移数不可超出 std::size_t 表示范围
			const auto shiftCount = static_cast<std::size_t>(other);
			if (shiftCount >= OtherBitSize)
			{
				return FixedInt{};
			}
			else
			{
				if constexpr (std::is_array_v<ValueType>)
				{
					FixedInt result{};
					constexpr auto ElemBitSize = sizeof(ElemType) * CHAR_BIT;
					const auto skipElements = shiftCount / ElemBitSize;
					const auto bitOffset = shiftCount % ElemBitSize;
					const auto overflowedShiftCount = ElemBitSize - bitOffset;
					const auto neverOverflow = bitOffset == 0;
					const auto overflowedMask = neverOverflow
					                                ? ~std::size_t()
					                                : (std::size_t(1) << overflowedShiftCount) - 1;
					ElemType overflowedElem{};
					const auto range = ForEachElementIndexesRange<IterateOrder::LowToHigh>() |
					                   std::views::take(std::extent_v<ValueType> - skipElements);
					for (auto iter = range.begin(), end = range.end(); iter != end; ++iter)
					{
						const auto fromElem = Value[*iter];
						auto& toElem = result.Value[*std::next(iter, skipElements)];
						toElem = (fromElem << bitOffset) | overflowedElem;
						overflowedElem =
						    neverOverflow ? 0 : (fromElem >> overflowedShiftCount) & overflowedMask;
					}
					return result;
				}
				else
				{
					return FixedInt(Value << shiftCount);
				}
			}
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt& operator<<=(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) noexcept
		{
			return *this = *this << other;
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt operator>>(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			// 位移数不可超出 std::size_t 表示范围
			const auto shiftCount = static_cast<std::size_t>(other);
			if (shiftCount >= OtherBitSize)
			{
				return IsNegative() ? ~FixedInt{} : FixedInt{};
			}
			else
			{
				if constexpr (std::is_array_v<ValueType>)
				{
					FixedInt result{};
					constexpr auto ElemBitSize = sizeof(ElemType) * CHAR_BIT;
					const auto skipElements = shiftCount / ElemBitSize;
					const auto bitOffset = shiftCount % ElemBitSize;
					const auto overflowedShiftCount = ElemBitSize - bitOffset;
					const auto neverOverflow = bitOffset == 0;
					const auto overflowedMask = neverOverflow
					                                ? ~std::size_t()
					                                : (std::size_t(1) << overflowedShiftCount) - 1;
					auto overflowedElem{ IsNegative() ? ~ElemType{} : ElemType{} };
					const auto range = ForEachElementIndexesRange<IterateOrder::HighToLow>() |
					                   std::views::take(std::extent_v<ValueType> - skipElements);
					for (auto iter = range.begin(), end = range.end(); iter != end; ++iter)
					{
						const auto fromElem = Value[*iter];
						auto& toElem = result.Value[*std::next(iter, skipElements)];
						toElem = (fromElem >> bitOffset) | (overflowedElem << overflowedShiftCount);
						overflowedElem = neverOverflow ? 0 : fromElem & overflowedMask;
					}
					return result;
				}
				else
				{
					return FixedInt(Value >> shiftCount);
				}
			}
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt& operator>>=(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) noexcept
		{
			return *this = *this >> other;
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr auto operator&(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return BinaryOperation<IterateOrder::LowToHigh>(
			    other, [](auto& result, auto a, auto b) { result = a & b; });
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt& operator&=(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) noexcept
		{
			return *this = FixedInt(*this & other);
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr auto operator|(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return BinaryOperation<IterateOrder::LowToHigh>(
			    other, [](auto& result, auto a, auto b) { result = a | b; });
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt& operator|=(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) noexcept
		{
			return *this = FixedInt(*this | other);
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr auto operator^(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return BinaryOperation<IterateOrder::LowToHigh>(
			    other, [](auto& result, auto a, auto b) { result = a ^ b; });
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr FixedInt& operator^=(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) noexcept
		{
			return *this = FixedInt(*this ^ other);
		}

		template <std::size_t OtherBitSize, std::endian OtherEndian,
		          std::integral... OtherUnderlyingInts>
		constexpr auto operator*(
		    FixedInt<OtherBitSize, OtherEndian, OtherUnderlyingInts...> const& other) const noexcept
		{
			return UsualArithmeticConversion(
			    other, [&]<std::size_t CommonBitSize, std::endian CommonEndian,
			               std::integral... AUnderlyingInts, std::integral... BUnderlyingInts>(
			               FixedInt<CommonBitSize, CommonEndian, AUnderlyingInts...> const& a,
			               FixedInt<CommonBitSize, CommonEndian, BUnderlyingInts...> const& b) {
					// 结果的 BitSize 不大于 a 的 BitSize + b 的 BitSize 即 CommonBitSize + CommonBitSize
					FixedInt<CommonBitSize * 2, CommonEndian, AUnderlyingInts...> result{};
					for (auto& resultElem : result.ForEachElementsRange<IterateOrder::LowToHigh>())
					{
						for (const auto elemA : a.ForEachElementsRange<IterateOrder::LowToHigh>())
						{
							for (const auto elemB : b.ForEachElementsRange<IterateOrder::LowToHigh>())
							{
								const auto& carry = resultElem;
								// TODO
								resultElem = elemA * elemB + carry;
							}
						}
					}
					return result;
			    });
		}
	};

	template <std::size_t BitSize, std::endian Endian, std::integral... UnderlyingInts,
	          typename Serializer>
	    requires SerializableType<typename FixedInt<BitSize, Endian, UnderlyingInts...>::ValueType,
	                              Serializer>
	struct SerializableTypeTraits<FixedInt<BitSize, Endian, UnderlyingInts...>, Serializer>
	{
		static constexpr void SerializeTo(FixedInt<BitSize, Endian, UnderlyingInts...> const& obj,
		                                  Serializer& serializer)
		{
			SerializableTypeTraits<typename FixedInt<BitSize, Endian, UnderlyingInts...>::ValueType,
			                       Serializer>::SerializeTo(obj.Value, serializer);
		}
	};

	// 应当自动实现 DeserializeConstructibleType<FixedInt<BitSize, Endian, UnderlyingInts...>, Deserializer>

	template <std::size_t BitSize, std::endian Endian, std::integral... UnderlyingInts,
	          typename Deserializer>
	    requires DeserializeAssignableType<
	        typename FixedInt<BitSize, Endian, UnderlyingInts...>::ValueType, Deserializer>
	struct DeserializeAssignableTypeTraits<FixedInt<BitSize, Endian, UnderlyingInts...>,
	                                       Deserializer>
	{
		static constexpr void
		DeserializeAssignFrom(FixedInt<BitSize, Endian, UnderlyingInts...>& obj,
		                      Deserializer& deserializer)
		{
			DeserializeAssignableTypeTraits<
			    typename FixedInt<BitSize, Endian, UnderlyingInts...>::ValueType,
			    Deserializer>::DeserializeAssignFrom(obj.Value, deserializer);
		}
	};

	template <std::size_t BitSize, std::endian Endian = std::endian::native>
	using FixedSInt =
	    FixedInt<BitSize, Endian, std::int8_t, std::int16_t, std::int32_t, std::int64_t>;

	template <std::size_t BitSize, std::endian Endian = std::endian::native>
	using FixedUInt =
	    FixedInt<BitSize, Endian, std::uint8_t, std::uint16_t, std::uint32_t, std::uint64_t>;

	inline namespace IntLiterals
	{
		namespace Detail
		{
			template <auto Result, char... Digits>
			struct ToFixedInt;

			// 给定 Int ∈ [1, +∞)，FirstDigit ∈ [0, 9]，则
			// Int * 10 + FirstDigit <= Int * 11 < Int * 16 == Int << 4
			// 即每次累加 FirstDigit，Int 不会增长 BitSize 超过 4
			template <std::size_t BitSize, std::endian Endian, std::integral... UnderlyingInts,
			          FixedInt<BitSize, Endian, UnderlyingInts...> Int, char FirstDigit,
			          char... RestDigits>
			struct ToFixedInt<Int, FirstDigit, RestDigits...>
			{
			};
		} // namespace Detail

		// 使得 BitSize 是 EffectiveIntBitSize 的整数倍
		// template <char... Digits>
		// constexpr auto operator""_I() noexcept
		// {
		// 	TODO
		// }
	} // namespace IntLiterals

	template <typename T>
	struct GetIntBitSizeTraits;

	template <std::size_t BitSize, std::endian Endian, std::integral... UnderlyingInts>
	struct GetIntBitSizeTraits<FixedInt<BitSize, Endian, UnderlyingInts...>>
	    : std::integral_constant<std::size_t, BitSize>
	{
	};

	template <typename... IntegralConstants>
	struct IntegralConstantSum;

	template <typename T, T... Values>
	struct IntegralConstantSum<std::integral_constant<T, Values>...>
	    : std::integral_constant<T, (T() + ... + Values)>
	{
	};

	// 由多个不同位数的整数组合成的整数，结果将会对齐到字节，不同整数之间严格按位邻接
	template <std::endian Endian, typename... Ints>
	struct ComposedInt
	{
		using ElementsTuple = std::tuple<Ints...>;
		static constexpr auto PrefixBitSizes = TypeSequenceToArray<
		    typename ForAllPrefix<std::tuple<std::integral_constant<std::size_t, 0>,
		                                     typename GetIntBitSizeTraits<Ints>::type...>,
		                          IntegralConstantSum, std::tuple>::type,
		    std::type_identity_t>::value;
		static constexpr std::size_t TotalBitSize = (... + GetIntBitSizeTraits<Ints>::value);
		using UnderlyingIntType = FixedUInt<TotalBitSize, Endian>;

		UnderlyingIntType Value;

		template <std::size_t I>
		constexpr auto Get() const noexcept
		{
			using ResultType = std::tuple_element_t<I, ElementsTuple>;
			constexpr auto BitSize = GetIntBitSizeTraits<ResultType>::value;
			constexpr auto PrefixBitSize = PrefixBitSizes[I];
			// TODO: 处理溢出
			constexpr auto Mask =
			    (UnderlyingIntType::FromIntegral(1) << UnderlyingIntType::FromIntegral(BitSize)) -
			    UnderlyingIntType::FromIntegral(1);
			return ResultType(
			    (Value >> UnderlyingIntType::FromIntegral(TotalBitSize - BitSize - PrefixBitSize)) &
			    Mask);
		}

		template <std::size_t I>
		constexpr void Set(std::tuple_element_t<I, ElementsTuple> const& value) noexcept
		{
			constexpr auto BitSize =
			    GetIntBitSizeTraits<std::tuple_element_t<I, ElementsTuple>>::value;
			constexpr auto PostfixBitSize = TotalBitSize - PrefixBitSizes[I + 1];
			// TODO: 处理溢出
			const auto Mask = ~(
			    ((UnderlyingIntType::FromIntegral(1) << UnderlyingIntType::FromIntegral(BitSize)) -
			     UnderlyingIntType::FromIntegral(1))
			    << UnderlyingIntType::FromIntegral(PostfixBitSize));
			Value = UnderlyingIntType(
			    (Value & Mask) |
			    (UnderlyingIntType(value) << UnderlyingIntType::FromIntegral(PostfixBitSize)));
		}
	};

	template <std::size_t Size>
	struct Padding
	{
	};

	template <StaticString Name, typename T>
	struct Field
	{
		using Type = T;
	};

	template <typename Field>
	struct FieldContainer;

	template <StaticString Name, typename T>
	struct FieldContainer<Field<Name, T>>
	{
		T Content;
	};

	template <StaticString Name, typename T, typename Serializer>
	    requires SerializableType<T, Serializer>
	struct SerializableTypeTraits<FieldContainer<Field<Name, T>>, Serializer>
	{
		static constexpr void SerializeTo(FieldContainer<Field<Name, T>> const& obj,
		                                  Serializer& serializer)
		{
			SerializableTypeTraits<T, Serializer>::SerializeTo(obj.Content, serializer);
		}
	};

	template <StaticString Name, typename T, typename Deserializer>
	    requires DeserializeConstructibleType<T, Deserializer>
	struct DeserializeConstructibleTypeTraits<FieldContainer<Field<Name, T>>, Deserializer>
	{
		static constexpr FieldContainer<Field<Name, T>>
		DeserializeConstructFrom(Deserializer& deserializer)
		{
			return {
				{ DeserializeConstructibleTypeTraits<T, Deserializer>::DeserializeConstructFrom(
				    deserializer) }
			};
		}
	};

	template <StaticString Name, typename T, typename Deserializer>
	    requires DeserializeAssignableType<T, Deserializer>
	struct DeserializeAssignableTypeTraits<FieldContainer<Field<Name, T>>, Deserializer>
	{
		static constexpr void DeserializeAssignFrom(FieldContainer<Field<Name, T>>& obj,
		                                            Deserializer& deserializer)
		{
			DeserializeAssignableTypeTraits<T, Deserializer>::DeserializeAssignFrom(obj.Content,
			                                                                        deserializer);
		}
	};

	// 视图不具有对象的所有权
	template <typename Field>
	struct FieldView;

	// 为可平凡构造及复制的对象自动实现默认的视图
	template <StaticString Name, typename T>
	    requires std::is_trivially_constructible_v<T> && std::is_trivially_copyable_v<T>
	struct FieldView<Field<Name, T>>
	{
		std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>, sizeof(T)>
		    Span;

		constexpr T Value() const noexcept
		{
			std::remove_const_t<T> obj{};
			std::memcpy(std::addressof(obj), Span.data(), sizeof(T));
			return obj;
		}

		template <typename Type = T>
		    requires(!std::is_const_v<Type>)
		constexpr void SetValue(Type value) const noexcept
		{
			std::memcpy(Span.data(), std::addressof(value), sizeof(Type));
		}
	};

	template <StaticString Name, typename T, typename Serializer>
	    requires std::is_trivially_constructible_v<T> && std::is_trivially_copyable_v<T> &&
	             SerializableType<
	                 std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>,
	                           sizeof(T)>,
	                 Serializer>
	struct SerializableTypeTraits<FieldView<Field<Name, T>>, Serializer>
	{
		static constexpr void SerializeTo(FieldView<Field<Name, T>> const& obj,
		                                  Serializer& serializer)
		{
			SerializableTypeTraits<
			    std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>,
			              sizeof(T)>,
			    Serializer>::SerializeTo(obj.Span, serializer);
		}
	};

	template <StaticString Name, typename T, typename Deserializer>
	    requires std::is_trivially_constructible_v<T> && std::is_trivially_copyable_v<T> &&
	             DeserializeConstructibleType<
	                 std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>,
	                           sizeof(T)>,
	                 Deserializer>
	struct DeserializeConstructibleTypeTraits<FieldView<Field<Name, T>>, Deserializer>
	{
		static constexpr FieldView<Field<Name, T>>
		DeserializeConstructFrom(Deserializer& deserializer)
		{
			return { { DeserializeConstructibleTypeTraits<
				std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>,
				          sizeof(T)>,
				Deserializer>::DeserializeConstructFrom(deserializer) } };
		}
	};

	template <StaticString Name, typename T, typename Deserializer>
	    requires std::is_trivially_constructible_v<T> && std::is_trivially_copyable_v<T> &&
	             DeserializeAssignableType<
	                 std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>,
	                           sizeof(T)>,
	                 Deserializer>
	struct DeserializeAssignableTypeTraits<FieldView<Field<Name, T>>, Deserializer>
	{
		static constexpr void DeserializeAssignFrom(FieldView<Field<Name, T>>& obj,
		                                            Deserializer& deserializer)
		{
			DeserializeAssignableTypeTraits<
			    std::span<std::conditional_t<std::is_const_v<T>, const std::byte, std::byte>,
			              sizeof(T)>,
			    Deserializer>::DeserializeAssignFrom(obj.Span, deserializer);
		}
	};

	namespace Detail
	{
		template <typename Tuple, StaticString Name>
		struct FindIndex
		{
			template <typename Field>
			struct IsNameEquals;

			template <template <typename> class FieldRepresentation, StaticString FieldName,
			          typename T>
			struct IsNameEquals<FieldRepresentation<Field<FieldName, T>>>
			    : std::bool_constant<FieldName == Name>
			{
			};

			static constexpr std::size_t Index = FindFirstIndexOf<Tuple, IsNameEquals>::value;
		};
	} // namespace Detail

	template <template <typename> class FieldRepresentation, typename... Fields>
	struct ComposedObject
	{
		using ContentsType = std::tuple<FieldRepresentation<Fields>...>;
		ContentsType Contents;

		// template <StaticString Name, typename Self>
		// constexpr decltype(auto) GetField(this Self&& self) noexcept
		// {
		// 	return std::get<Detail::FindIndex<ContentsType, Name>::Index>(
		// 	    std::forward<Self>(self).Contents);
		// }

		template <StaticString Name>
		constexpr decltype(auto) GetField() noexcept
		{
			return std::get<Detail::FindIndex<ContentsType, Name>::Index>(Contents);
		}

		template <StaticString Name>
		constexpr decltype(auto) GetField() const noexcept
		{
			return std::get<Detail::FindIndex<ContentsType, Name>::Index>(Contents);
		}
	};

	template <template <typename> class FieldRepresentation, typename... Fields,
	          typename Serializer>
	    requires(... && SerializableType<FieldRepresentation<Fields>, Serializer>)
	struct SerializableTypeTraits<ComposedObject<FieldRepresentation, Fields...>, Serializer>
	{
		static constexpr void SerializeTo(ComposedObject<FieldRepresentation, Fields...> const& obj,
		                                  Serializer& serializer)
		{
			[&]<std::size_t... Indexes>(std::index_sequence<Indexes...>) {
				(..., SerializableTypeTraits<FieldRepresentation<Fields>, Serializer>::SerializeTo(
				          std::get<Indexes>(obj.Contents), serializer));
			}(std::make_index_sequence<sizeof...(Fields)>{});
		}
	};

	template <template <typename> class FieldRepresentation, typename... Fields,
	          typename Deserializer>
	    requires(... && DeserializeConstructibleType<FieldRepresentation<Fields>, Deserializer>)
	struct DeserializeConstructibleTypeTraits<ComposedObject<FieldRepresentation, Fields...>,
	                                          Deserializer>
	{
		static constexpr ComposedObject<FieldRepresentation, Fields...>
		DeserializeConstructFrom(Deserializer& deserializer)
		{
			return ComposedObject<FieldRepresentation, Fields...>{
				{ DeserializeConstructibleTypeTraits<FieldRepresentation<Fields>, Deserializer>::
				      DeserializeConstructFrom(deserializer)... }
			};
		}
	};

	template <template <typename> class FieldRepresentation, typename... Fields,
	          typename Deserializer>
	    requires(... && DeserializeAssignableType<FieldRepresentation<Fields>, Deserializer>)
	struct DeserializeAssignableTypeTraits<ComposedObject<FieldRepresentation, Fields...>,
	                                       Deserializer>
	{
		static constexpr void
		DeserializeAssignFrom(ComposedObject<FieldRepresentation, Fields...>& obj,
		                      Deserializer& deserializer)
		{
			[&]<std::size_t... Indexes>(std::index_sequence<Indexes...>) {
				(..., DeserializeAssignableTypeTraits<FieldRepresentation<Fields>, Deserializer>::
				          DeserializeAssignFrom(std::get<Indexes>(obj.Contents), deserializer));
			}(std::make_index_sequence<sizeof...(Fields)>{});
		}
	};

	template <typename... Fields>
	using Object = ComposedObject<FieldContainer, Fields...>;

	template <typename... Fields>
	using View = ComposedObject<FieldView, Fields...>;
} // namespace Espresso

#endif
