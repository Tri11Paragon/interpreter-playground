#include <iostream>
#include <blt/logging/logging.h>


struct tokenizer_t
{
	struct token_t
	{
		size_t begin;
		size_t end;

		token_t(const size_t begin, const size_t end): begin{begin},
													   end{end} {}
	};


	enum tokens_t
	{};
};


int main() { std::cout << "Hello World!" << std::endl; }
