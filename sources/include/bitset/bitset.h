#ifndef bitset_h
#define bitset_h

#include <utils/utils.h>

#include <array>
#include <boost/leaf.hpp>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <utility>

namespace utils
{
namespace leaf = boost::leaf;

template <class T>
using result = leaf::result<T>;

using e_source_location = leaf::e_source_location;

enum class eBitsetError
{
    out_of_range
};

namespace details
{
namespace bitset
{
inline constexpr void validate_shift_args(
    [[maybe_unused]] std::byte* aDst, [[maybe_unused]] std::byte const* aSrc,
    [[maybe_unused]] const std::size_t aNumBits) noexcept
{
    assert(aDst);
    assert(aSrc);
    assert(aNumBits < CHAR_BIT);
}

using shifter_t = void (*)(std::byte*, std::byte const*,
                           const std::size_t) noexcept;

template <std::size_t NBytes>
constexpr void right_shift_bytes(std::byte* aDst, std::byte const* aSrc,
                                 const std::size_t aNumBits) noexcept
{
    static_assert(NBytes > 0);
    static_assert(NBytes <= sizeof(std::uint64_t));
    validate_shift_args(aDst, aSrc, aNumBits);
    integral_to_bytes<NBytes>(
        aDst, shift_right(bytes_to_integral<NBytes, FastUInt<NBytes>>(aSrc),
                          aNumBits));
}

template <std::size_t... I>
constexpr decltype(auto) make_right_shifter_array(
    std::index_sequence<I...>) noexcept
{
    static_assert(sizeof...(I) > 0);
    static_assert(sizeof...(I) <= sizeof(std::uint64_t));
    return std::array<shifter_t, sizeof...(I)>{right_shift_bytes<I + 1>...};
}

inline constexpr auto right_shifters =
    make_right_shifter_array(std::make_index_sequence<sizeof(std::uint64_t)>{});

using shifters_array_t = decltype(right_shifters);

template <std::size_t NBytes>
constexpr void left_shift_bytes(std::byte* aDst, std::byte const* aSrc,
                                const std::size_t aNumBits) noexcept
{
    static_assert(NBytes > 0);
    static_assert(NBytes <= sizeof(std::uint64_t));
    validate_shift_args(aDst, aSrc, aNumBits);
    integral_to_bytes<NBytes>(
        aDst, shift_left(bytes_to_integral<NBytes, FastUInt<NBytes>>(aSrc),
                         aNumBits));
}

template <std::size_t... I>
constexpr decltype(auto) make_left_shifter_array(
    std::index_sequence<I...>) noexcept
{
    static_assert(sizeof...(I) > 0);
    static_assert(sizeof...(I) <= sizeof(std::uint64_t));
    return std::array<shifter_t, sizeof...(I)>{left_shift_bytes<I + 1>...};
}

inline constexpr auto left_shifters =
    make_left_shifter_array(std::make_index_sequence<sizeof(std::uint64_t)>{});
}  // namespace bitset
}  // namespace details

template <std::size_t N>
class bitset final
{
   public:
    static constexpr std::size_t kNumBytes =
        N / CHAR_BIT + static_cast<bool>(N % CHAR_BIT);

    class reference final
    {
       public:
        constexpr reference(std::size_t aPos, bitset<N>& aBitset) noexcept
            : pos_(aPos), bitset_(aBitset)
        {
        }

        reference& operator=(bool aVal) noexcept
        {
            if (aVal)
            {
                bitset_.get().set(pos_);
            }
            else
            {
                bitset_.get().reset(pos_);
            }
            return *this;
        }

        reference& operator=(const reference& aVal) noexcept
        {
            *this = static_cast<bool>(aVal);
            return *this;
        }

        constexpr operator bool() const noexcept
        {
            const auto& bsref = bitset_.get();
            return bsref[pos_];
        }

        constexpr bool operator~() const noexcept { return !bitset_[pos_]; }

        constexpr reference& flip() noexcept
        {
            bitset_.flip(pos_);
            return *this;
        }

       private:
        const std::size_t pos_ = 0;
        std::reference_wrapper<bitset<N>> bitset_{};
    };

    constexpr bitset() noexcept = default;

    template <typename T>
    explicit constexpr bitset(T&& aBits) noexcept
        : data_(create_data_array(std::forward<T>(aBits)))
    {
    }

    constexpr bool operator[](std::size_t aPos) const noexcept
    {
        return std::to_integer<bool>(data_[aPos / CHAR_BIT] & bitAtIndex(aPos));
    }

    constexpr reference operator[](std::size_t aPos) noexcept
    {
        return {aPos, *this};
    }

    constexpr void set(std::size_t aPos) noexcept
    {
        data_[aPos / CHAR_BIT] |= bitAtIndex(aPos);
    }

    constexpr void reset(std::size_t aPos) noexcept
    {
        data_[aPos / CHAR_BIT] &= ~bitAtIndex(aPos);
    }

    constexpr bitset& flip(std::size_t aPos) noexcept
    {
        data_[aPos / CHAR_BIT] ^= bitAtIndex(aPos);
        return *this;
    }

    constexpr bitset& flip() noexcept
    {
        if constexpr (N % CHAR_BIT == 0)
        {
            flipBits(std::make_index_sequence<kNumBytes>{});
        }
        else
        {
            flipBits(std::make_index_sequence<kNumBytes - 1>{});
            data_[kNumBytes - 1] ^= cutUnrelatedMask();
        }
        return *this;
    }

    constexpr std::size_t size() const noexcept { return N; }

    constexpr std::size_t count() const noexcept
    {
        using Indices = std::make_index_sequence<kNumBytes>;
        return bitsCount(Indices{});
    }

    constexpr bool all() const noexcept { return size() == count(); }

    constexpr bool any() const noexcept
    {
        using Indices = std::make_index_sequence<kNumBytes>;
        return isAnyBitSet(Indices{});
    }

    constexpr bool none() const noexcept { return 0 == count(); }

    constexpr bool operator==(const bitset& aRhs) const noexcept
    {
        using Indices = std::make_index_sequence<kNumBytes>;
        return isEqual(aRhs, Indices{});
    }

    constexpr bool operator!=(const bitset& aRhs) const noexcept
    {
        return !(*this == aRhs);
    }

    constexpr bitset& operator&=(const bitset& aOther) noexcept
    {
        using Indices = std::make_index_sequence<kNumBytes>;
        bitAND(aOther, Indices{});
        return *this;
    }

    constexpr bitset& operator|=(const bitset& aOther) noexcept
    {
        using Indices = std::make_index_sequence<kNumBytes>;
        bitOR(aOther, Indices{});
        return *this;
    }

    constexpr bitset& operator^=(const bitset& aOther) noexcept
    {
        using Indices = std::make_index_sequence<kNumBytes>;
        bitXOR(aOther, Indices{});
        return *this;
    }

    constexpr bitset operator~() const noexcept
    {
        if constexpr (N % CHAR_BIT != 0)
        {
            using Indices = std::make_index_sequence<kNumBytes - 1>;
            return bitset<N>(
                make_flipped_array(Indices{}, data_[kNumBytes - 1]));
        }
        else
        {
            using Indices = std::make_index_sequence<kNumBytes>;
            return bitset<N>(make_flipped_array(Indices{}));
        }
    }

    result<bool> test(std::size_t aPos) const noexcept
    {
        if (aPos < this->size())
        {
            return this->operator[](aPos);
        }
        else
        {
            return BOOST_LEAF_NEW_ERROR(eBitsetError::out_of_range,
                                        this->size(), aPos);
        }
    }

    constexpr bitset operator<<(std::size_t aPos) const noexcept
    {
        assert(aPos <= this->size());
        bitset ret;
        if (aPos < this->size())
        {
            left_shift_helper(ret.data_.data(), aPos);
        }
        return ret;
    }

    constexpr bitset& operator<<=(const std::size_t aPos) noexcept
    {
        assert(aPos <= this->size());
        if (aPos < this->size())
        {
            const auto zero_count = left_shift_helper(data_.data(), aPos);
            memory::secure_zero_memory(data_.data(), zero_count);
        }
        else
        {
            memory::secure_zero_memory(data_.data(), data_.size());
        }
        return *this;
    }

    constexpr bitset operator>>(std::size_t aPos) const noexcept
    {
        assert(aPos <= this->size());
        bitset ret;
        if (aPos < this->size())
        {
            right_shift_helper(ret.data_.data(), aPos);
        }
        return ret;
    }

    constexpr bitset& operator>>=(const std::size_t aPos) noexcept
    {
        assert(aPos <= this->size());
        if (aPos < this->size())
        {
            const auto [zero_count, bytes_to_shift] =
                right_shift_helper(data_.data(), aPos);
            memory::secure_zero_memory(data_.data() + bytes_to_shift,
                                       zero_count);
        }
        else
        {
            memory::secure_zero_memory(data_.data(), data_.size());
        }
        return *this;
    }

    constexpr std::byte const* data() const noexcept { return data_.data(); }

   private:
    using shifters_array_t = details::bitset::shifters_array_t;

    constexpr bitset(std::array<std::byte, kNumBytes> aData) noexcept
        : data_(aData)
    {
    }

    constexpr std::size_t left_shift_helper(std::byte* aDst,
                                            std::size_t aPos) const noexcept
    {
        using details::bitset::left_shifters;
        const auto [quot, rem] = div(static_cast<int>(aPos), CHAR_BIT);
        std::size_t bytes_to_shift =
            data_.size() - static_cast<std::size_t>(quot);
        auto dst = aDst + quot;
        shift_helper(dst, data(), bytes_to_shift, rem, left_shifters,
                     left_shifter);
        return static_cast<std::size_t>(quot);
    }

    constexpr decltype(auto) right_shift_helper(std::byte* aDst,
                                                std::size_t aPos) const noexcept
    {
        using details::bitset::right_shifters;
        const auto [quot, rem] = div(static_cast<int>(aPos), CHAR_BIT);
        std::size_t bytes_to_shift =
            data_.size() - static_cast<std::size_t>(quot);
        auto src = this->data() + quot;
        shift_helper(aDst, src, bytes_to_shift, rem, right_shifters,
                     right_shifter);
        return std::make_tuple(static_cast<std::size_t>(quot), bytes_to_shift);
    }

    template <typename T>
    static constexpr void shift_helper(std::byte* aDst, std::byte const* aSrc,
                                       std::size_t aBytesToShift, int aRem,
                                       shifters_array_t& aShifters,
                                       T&& aShiftOp) noexcept
    {
        if (aRem)
        {
            const std::size_t bytes_to_write = bytes_to_read - 1;
            while (aBytesToShift > bytes_to_read)
            {
                auto value = aShiftOp(
                    bytes_to_integral<bytes_to_read, FastUInt<bytes_to_read>>(
                        aSrc),
                    static_cast<std::size_t>(aRem));
                integral_to_bytes<bytes_to_write>(aDst, std::move(value));
                aBytesToShift -= bytes_to_write;
                aSrc += bytes_to_write;
                aDst += bytes_to_write;
            }
            aShifters[aBytesToShift - 1](aDst, aSrc,
                                         static_cast<std::size_t>(aRem));
        }
        else
        {
            memmove(aDst, aSrc, aBytesToShift);
        }
    }

    template <typename T, std::size_t... I>
    constexpr std::byte byte_from_bits(std::size_t aByteIndex, T&& aBits,
                                       std::index_sequence<I...>) noexcept
    {
        static_assert(sizeof...(I) > 0, "sizeof...(I) must be greater than 0");
        static_assert(sizeof...(I) <= CHAR_BIT,
                      "sizeof...(I) must be less or equal than CHAR_BIT(8)");
        auto to_char = [](auto&& aValue) -> char
        {
            constexpr bool is_byte =
                std::is_same_v<utils::remove_cvref_t<decltype(aValue)>,
                               std::byte>;
            constexpr bool is_char =
                std::is_same_v<utils::remove_cvref_t<decltype(aValue)>, char>;
            static_assert(is_byte || is_char, "aValue has unsupported type");
            if constexpr (is_byte)
            {
                return std::to_integer<char>(aValue);
            }
            else
            {
                return aValue;
            }
        };

        return std::byte{
            (... | (aByteIndex * CHAR_BIT + I >= aBits.size()
                        ? std::byte{0}
                        : ((to_char(aBits[aBits.size() - 1 -
                                          aByteIndex * CHAR_BIT - I]) == '0')
                               ? std::byte{0}
                               : (std::byte{1} << I))))};
    }

    template <typename T, std::size_t... I>
    constexpr std::array<std::byte, sizeof...(I)> make_bitset_array(
        T&& aBits, std::index_sequence<I...>) noexcept
    {
        return {byte_from_bits(I, std::forward<T>(aBits),
                               std::make_index_sequence < (I < N / CHAR_BIT)
                                   ? CHAR_BIT
                                   : N % CHAR_BIT > {})...};
    }

    template <typename T>
    constexpr decltype(auto) create_data_array(T&& aBits) noexcept
    {
        static_assert(kNumBytes > 0, "kNumBytes must be > 0");
        using Indices = std::make_index_sequence<kNumBytes>;
        return make_bitset_array(std::forward<T>(aBits), Indices{});
    }

    inline constexpr std::byte bitAtIndex(std::size_t aPos) const noexcept
    {
        return std::byte{1} << (aPos % CHAR_BIT);
    }

    inline constexpr std::byte cutUnrelatedMask() const noexcept
    {
        return ~std::byte{0} >> (CHAR_BIT - N % CHAR_BIT);
    }

    inline constexpr std::byte lastByteValue() const noexcept
    {
        return data_[kNumBytes - 1] & cutUnrelatedMask();
    }

    template <std::size_t... I>
    constexpr bool isEqual(const bitset& aRhs,
                           std::index_sequence<I...>) const noexcept
    {
        return (true && ... && (data_[I] == aRhs.data_[I]));
    }

    template <std::size_t... I>
    constexpr std::size_t bitsCount(std::index_sequence<I...>) const noexcept
    {
        return (... + (kNumBitsTable[std::to_integer<std::size_t>(data_[I])]));
    }

    template <std::size_t... I>
    constexpr void flipBits(std::index_sequence<I...>) noexcept
    {
        ((data_[I] = ~data_[I]), ...);
    }

    template <std::size_t... I>
    constexpr bool isAnyBitSet(std::index_sequence<I...>) const noexcept
    {
        return (... || kNumBitsTable[std::to_integer<std::size_t>(data_[I])]);
    }

    template <std::size_t... I>
    constexpr void bitAND(const bitset& aOther,
                          std::index_sequence<I...>) noexcept
    {
        return ((data_[I] &= aOther[I]), ...);
    }

    template <std::size_t... I>
    constexpr void bitOR(const bitset& aOther,
                         std::index_sequence<I...>) noexcept
    {
        return ((data_[I] |= aOther[I]), ...);
    }

    template <std::size_t... I>
    constexpr void bitXOR(const bitset& aOther,
                          std::index_sequence<I...>) noexcept
    {
        return ((data_[I] ^= aOther[I]), ...);
    }

    template <std::size_t... I>
    constexpr decltype(auto) make_flipped_array(
        std::index_sequence<I...>) const noexcept
    {
        return std::array<std::byte, kNumBytes>{(~data_[I])...};
    }

    template <std::size_t... I>
    constexpr decltype(auto) make_flipped_array(
        std::index_sequence<I...>, const std::byte aLast) const noexcept
    {
        return std::array<std::byte, kNumBytes>{(~data_[I])...,
                                                aLast ^ cutUnrelatedMask()};
    }

    static inline constexpr auto bytes_to_read = sizeof(std::uint64_t);
    using ValueT =
        decltype(bytes_to_integral<bytes_to_read, FastUInt<bytes_to_read>>(
            nullptr));
    static inline constexpr auto left_shifter = &shift_left<ValueT>;
    static inline constexpr auto right_shifter = &shift_right<ValueT>;
    std::array<std::byte, kNumBytes> data_{};
};

template <std::size_t N>
constexpr bitset<N> operator&(const bitset<N>& aLhs,
                              const bitset<N>& aRhs) noexcept
{
    bitset<N> result;
    for (std::size_t i = 0; i < result.kNumBytes; ++i)
    {
        result.data_[i] = aLhs.data_[i] & aRhs.data_[i];
    }
    return result;
}

template <std::size_t N>
constexpr bitset<N> operator|(const bitset<N>& aLhs,
                              const bitset<N>& aRhs) noexcept
{
    bitset<N> result;
    for (std::size_t i = 0; i < result.kNumBytes; ++i)
    {
        result.data_[i] = aLhs.data_[i] | aRhs.data_[i];
    }
    return result;
}

template <std::size_t N>
constexpr bitset<N> operator^(const bitset<N>& aLhs,
                              const bitset<N>& aRhs) noexcept
{
    bitset<N> result;
    for (std::size_t i = 0; i < result.kNumBytes; ++i)
    {
        result.data_[i] = aLhs.data_[i] ^ aRhs.data_[i];
    }
    return result;
}
}  // namespace utils

#endif /* bitset_h */
