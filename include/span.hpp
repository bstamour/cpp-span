#ifndef BST_SPAN_HPP_
#define BST_SPAN_HPP_

//------------------------------------------------------------------------------

// An implementation of std::span, as described by
// http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2018/p0122r7.pdf
//
// No guarantee that it is perfect or bug-free.

//------------------------------------------------------------------------------

#include <array>
#include <cstddef>
#include <iterator>
#include <type_traits>
#include <utility>
#include <vector>

//==============================================================================
namespace bst {

inline constexpr std::ptrdiff_t dynamic_extent = -1;

//------------------------------------------------------------------------------

template <typename T, std::ptrdiff_t Extent> class span;

//==============================================================================
namespace detail {

template <typename T> struct is_span : std::false_type {
};
template <typename T, std::ptrdiff_t E>
struct is_span<span<T, E>> : std::true_type {
};

template <typename T> struct is_std_array : std::false_type {
};
template <typename T, std::size_t N>
struct is_std_array<std::array<T, N>> : std::true_type {
};

template <typename T, typename = std::void_t<>>
struct has_data_overload : std::false_type {
};
template <typename T>
struct has_data_overload<T,
                         std::void_t<decltype(std::data(std::declval<T&>()))>>
  : std::true_type {
};

template <typename T, typename = std::void_t<>>
struct has_size_overload : std::false_type {
};
template <typename T>
struct has_size_overload<T,
                         std::void_t<decltype(std::size(std::declval<T&>()))>>
  : std::true_type {
};

template <typename Container, typename ElementT>
constexpr bool container_can_be_used =
  !detail::is_span<Container>::value &&
  !detail::is_std_array<Container>::value &&
  !std::is_array_v<Container> &&
  detail::has_data_overload<Container>::value &&
  detail::has_size_overload<Container>::value &&
  std::is_convertible_v<std::remove_pointer_t<decltype(
                          std::data(std::declval<Container&>()))> (*)[],
                        ElementT (*)[]>;
} // namespace detail
//==============================================================================

template <typename T, std::ptrdiff_t Extent = bst::dynamic_extent> class span {
public:
  using element_type           = T;
  using value_type             = std::remove_cv_t<T>;
  using index_type             = std::ptrdiff_t;
  using difference_type        = std::ptrdiff_t;
  using pointer                = T*;
  using reference              = T&;
  using iterator               = T*;
  using const_iterator         = const T*;
  using reverse_iterator       = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  //---

  static inline constexpr std::ptrdiff_t extent = Extent;

  //---

  constexpr span() noexcept : data_{nullptr}, size_{0} {}

  constexpr span(pointer ptr, index_type count) : data_{ptr}, size_{count} {}

  constexpr span(pointer first, pointer last)
    : data_{first}, size_{last - first}
  {
  }

  template <
    std::size_t N,
    typename = std::enable_if_t<Extent == bst::dynamic_extent || N == Extent>>
  constexpr span(element_type (&arr)[N]) noexcept
    : data_{std::data(arr)}, size_{N}
  {
  }

  template <
    std::size_t N,
    typename = std::enable_if_t<Extent == bst::dynamic_extent || N == Extent>>
  constexpr span(std::array<std::remove_const_t<element_type>, N>& arr) noexcept
    : data_{std::data(arr)}, size_{N}
  {
  }

  template <
    std::size_t N,
    typename = std::enable_if_t<Extent == bst::dynamic_extent || N == Extent>>
  constexpr span(
    const std::array<std::remove_const_t<element_type>, N>& arr) noexcept
    : data_{std::data(arr)}, size_{N}
  {
  }

  template <typename Container,
            typename = std::enable_if_t<
              detail::container_can_be_used<Container, element_type>>>
  constexpr span(Container& cont)
    : data_{std::data(cont)}, size_{static_cast<index_type>(std::size(cont))}
  {
  }

  template <typename Container,
            typename = std::enable_if_t<
              detail::container_can_be_used<Container, element_type>>>
  constexpr span(const Container& cont)
    : data_{std::data(cont)}, size_{static_cast<index_type>(std::size(cont))}
  {
  }

  template <typename U,
            std::ptrdiff_t N,
            typename = std::enable_if_t<
              (Extent == bst::dynamic_extent || Extent == N) &&
              std::is_convertible_v<U (*)[], element_type (*)[]>>>
  constexpr span(const span<U, N>& s) noexcept
    : data_{s.data()}, size_{s.size()}
  {
  }

  constexpr span(const span&) noexcept = default;

  ~span() noexcept = default;

  //---

  constexpr span& operator=(const span&) noexcept = default;

  //---

  constexpr iterator begin() const noexcept { return data_; }
  constexpr const_iterator cbegin() const noexcept { return data_; }

  constexpr iterator end() const noexcept { return data_ + size_; }
  constexpr const_iterator cend() const noexcept { return data_ + size_; }

  //---

  constexpr reverse_iterator rbegin() const noexcept
  {
    return std::make_reverse_iterator(begin());
  }
  constexpr const_reverse_iterator crbegin() const noexcept
  {
    return std::make_reverse_iterator(cbegin());
  }

  constexpr reverse_iterator rend() const noexcept
  {
    return std::make_reverse_iterator(end());
  }
  constexpr const_reverse_iterator crend() const noexcept
  {
    return std::make_reverse_iterator(cend());
  }

  //---

  constexpr reference operator[](index_type idx) const { return data_[idx]; }
  constexpr reference operator()(index_type idx) const { return data_[idx]; }

  constexpr pointer data() const noexcept { return data_; }

  //---

  constexpr index_type size() const noexcept { return size_; }
  constexpr index_type size_bytes() const noexcept
  {
    return size() * sizeof(element_type);
  }

  constexpr bool empty() const noexcept { return size_ == 0; }

  //---

  template <std::ptrdiff_t Count>
  constexpr span<element_type, Count> first() const
  {
    return span<element_type, Count>{data_, Count};
  }
  constexpr span<element_type, bst::dynamic_extent>
  first(std::ptrdiff_t count) const
  {
    return span<element_type, bst::dynamic_extent>{data_, count};
  }

  template <std::ptrdiff_t Count>
  constexpr span<element_type, Count> last() const
  {
    return span<element_type, Count>{data_ + (size_ - Count), Count};
  }
  constexpr span<element_type, bst::dynamic_extent>
  last(std::ptrdiff_t count) const
  {
    return span<element_type, bst::dynamic_extent>{data_ + (size_ - count),
                                                   count};
  }

  template <std::ptrdiff_t Offset, std::ptrdiff_t Count = bst::dynamic_extent>
  constexpr auto subspan() const
  {
    if constexpr (Count != bst::dynamic_extent)
      return span<element_type, Count>{data_ + Offset, Count};
    else if constexpr (Extent != bst::dynamic_extent)
      return span<element_type, Extent - Offset>{data_ + Offset,
                                                 size_ - Offset};
    else
      return span<element_type, bst::dynamic_extent>{data_ + Offset,
                                                     size_ - Offset};
  }

  constexpr span<element_type, bst::dynamic_extent>
  subspan(std::ptrdiff_t offset,
          std::ptrdiff_t count = bst::dynamic_extent) const
  {
    if (count != bst::dynamic_extent)
      return span<element_type, bst::dynamic_extent>{data_ + offset, count};
    else
      return span<element_type, bst::dynamic_extent>{data_ + offset,
                                                     size_ - offset};
  }

private:
  pointer data_;
  index_type size_;
};

//------------------------------------------------------------------------------

template <typename T, std::size_t N> span(T (&)[N])->span<T, N>;

template <typename T, std::size_t N> span(std::array<T, N>&)->span<T, N>;

template <typename T, std::size_t N>
span(const std::array<T, N>&)->span<const T, N>;

template <typename Container>
span(Container&)->span<typename Container::value_type>;

template <typename Container>
span(const Container&)->span<const typename Container::value_type>;

//------------------------------------------------------------------------------

#if 0
template <typename T, std::ptrdiff_t E>
constexpr bool operator==(span<T, E> lhs, span<T, E> rhs)
{
  return true;
}
#endif

//------------------------------------------------------------------------------

template <typename T, std::ptrdiff_t Extent>
constexpr auto begin(const span<T, Extent>& s) noexcept
{
  return s.begin();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto end(const span<T, Extent>& s) noexcept
{
  return s.end();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto cbegin(const span<T, Extent>& s) noexcept
{
  return s.cbegin();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto cend(const span<T, Extent>& s) noexcept
{
  return s.cend();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto rbegin(const span<T, Extent>& s) noexcept
{
  return s.rbegin();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto rend(const span<T, Extent>& s) noexcept
{
  return s.rend();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto crbegin(const span<T, Extent>& s) noexcept
{
  return s.crbegin();
}

template <typename T, std::ptrdiff_t Extent>
constexpr auto crend(const span<T, Extent>& s) noexcept
{
  return s.crend();
}

template <typename T, std::ptrdiff_t N> auto as_bytes(span<T, N> s) noexcept
{
  if constexpr (N != bst::dynamic_extent)
    return span<const std::byte, (static_cast<std::ptrdiff_t>(sizeof(T)) * N)>{
      reinterpret_cast<const std::byte*>(s.data()), s.size_bytes()};
  else
    return span<const std::byte, bst::dynamic_extent>{
      reinterpret_cast<const std::byte*>(s.data()), s.size_bytes()};
}

template <typename T,
          std::ptrdiff_t N,
          typename = std::enable_if_t<!std::is_const_v<T>>>
auto as_writable_bytes(span<T, N> s) noexcept
{
  if constexpr (N != bst::dynamic_extent)
    return span<std::byte, (static_cast<std::ptrdiff_t>(sizeof(T)) * N)>{
      reinterpret_cast<std::byte*>(s.data()), s.size_bytes()};
  else
    return span<std::byte, bst::dynamic_extent>{
      reinterpret_cast<std::byte*>(s.data()), s.size_bytes()};
}

} // namespace bst
//==============================================================================

#endif
