Hierarchical Summarization
Hierarchical summarization condenses long documents into layered summaries (like a pyramid) to preserve key information while minimizing tokens

Let’s say you want to build a conversational system, for your system to be powerful and reliable you want it to remember past discussions and manage the history of conversations so it provides contextualized answers for your users. Hierarchical summarization can be your go to in this use case.

How it works:

The process starts by dividing the input text into smaller chunks, such as paragraphs, sections, or documents.

Each chunk is then summarized individually ⇒ These are the first level of the hierarchy

The summaries from the previous level are then combined and summarized again, creating a higher-level summary.

This process can be repeated multiple times, creating a hierarchy of summaries with each level representing a more condensed view of the input.

example :

from transformers import pipeline
# Initialize summarization pipeline
summarizer = pipeline("summarization", model="facebook/bart-large-cnn")
def hierarchical_summarize(text, levels=3, max_length=150):
	for _ in range(levels):
		text = summarizer(text, max_length=max_length)[0]['summary_text']
	return text
Usage:
long_document = "..."  # Your input text/ retrieve conversation history 
final_summary = hierarchical_summarize(long_document, levels=3)