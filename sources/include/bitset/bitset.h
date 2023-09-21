#ifndef bitset_h
#define bitset_h

#include <utils/utils.h>

#include <climits>
#include <cstddef>
#include <cstdint>

namespace utils
{
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
        return data_[aPos / CHAR_BIT] & bitAtIndex(aPos);
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

   private:
    constexpr bitset(std::array<uint8_t, kNumBytes> aData) noexcept
        : data_(aData)
    {
    }

    template <typename T, std::size_t... I>
    constexpr uint8_t byte_from_bits(std::size_t aByteIndex, T&& aBits,
                                     std::index_sequence<I...>) noexcept
    {
        static_assert(sizeof...(I) > 0, "sizeof...(I) must be greater than 0");
        static_assert(sizeof...(I) <= CHAR_BIT,
                      "sizeof...(I) must be less or equal than CHAR_BIT(8)");
        return static_cast<uint8_t>((
            ... |
            (aByteIndex * CHAR_BIT + I >= aBits.size()
                 ? 0
                 : ((aBits[aBits.size() - 1 - aByteIndex * CHAR_BIT - I] == '0')
                        ? 0
                        : (1 << I)))));
    }

    template <typename T, std::size_t... I>
    constexpr std::array<uint8_t, sizeof...(I)> make_bitset_array(
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

    inline constexpr uint8_t bitAtIndex(std::size_t aPos) const noexcept
    {
        return static_cast<uint8_t>(uint8_t(1) << (aPos % CHAR_BIT));
    }

    inline constexpr uint8_t cutUnrelatedMask() const noexcept
    {
        return static_cast<uint8_t>(uint8_t(~0) >> (CHAR_BIT - N % CHAR_BIT));
    }

    inline constexpr uint8_t lastByteValue() const noexcept
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
        return (... + (kNumBitsTable[data_[I]]));
    }

    template <std::size_t... I>
    constexpr void flipBits(std::index_sequence<I...>) noexcept
    {
        ((data_[I] = ~data_[I]), ...);
    }

    template <std::size_t... I>
    constexpr bool isAnyBitSet(std::index_sequence<I...>) const noexcept
    {
        return (... || kNumBitsTable[data_[I]]);
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
        return std::array<uint8_t, kNumBytes>{
            static_cast<uint8_t>(~data_[I])...};
    }

    template <std::size_t... I>
    constexpr decltype(auto) make_flipped_array(
        std::index_sequence<I...>, const uint8_t aLast) const noexcept
    {
        return std::array<uint8_t, kNumBytes>{
            static_cast<uint8_t>(~data_[I])...,
            static_cast<uint8_t>(aLast ^ cutUnrelatedMask())};
    }

    std::array<uint8_t, kNumBytes> data_{};
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
