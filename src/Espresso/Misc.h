#ifndef ESPRESSO_MISC_H
#define ESPRESSO_MISC_H

#include <memory>
#include <array>

namespace Espresso
{
	template <typename Char, std::size_t Size>
	struct StaticString
	{
		constexpr StaticString(const Char (&content)[Size]) : Name()
		{
			for (std::size_t i = 0; i < Size; ++i)
			{
				std::construct_at(Name + i, content[i]);
			}
		}

		template <Char... Chars>
		    requires(sizeof...(Chars) <= Size)
		constexpr StaticString(std::integer_sequence<Char, Chars...>) : Name{ Chars... }
		{
		}

		template <std::size_t OtherSize>
		constexpr bool operator==(StaticString<Char, OtherSize> const& other) const
		{
			if constexpr (Size != OtherSize)
			{
				return false;
			}
			else
			{
				return std::equal(std::begin(Name), std::end(Name), std::begin(other.Name));
			}
		}

		Char Name[Size];
	};

	template <typename Char, Char... Chars>
	StaticString(std::integer_sequence<Char, Chars...>) -> StaticString<Char, sizeof...(Chars) + 1>;

	template <typename Array>
	struct MakeStdArrayTraits;

	template <typename T, std::size_t Extent>
	struct MakeStdArrayTraits<T[Extent]> : std::type_identity<std::array<T, Extent>>
	{
	};

	template <typename Array>
	using MakeStdArray = typename MakeStdArrayTraits<Array>::type;
} // namespace Espresso

#endif
