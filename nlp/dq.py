import regex
import spacy
import nltk
import io

nlp = spacy.load('es_core_news_md')

def get_book_string(file_name):
    f = io.open(file_name, mode='r', encoding='utf-8-sig')
    return f.read()

def chapterize(book):
    split_chapters = regex.compile(u'Capítulo \w+\. ')
    return split_chapters.split(book)

def tag_and_text(chapter):
    lowered = chapter.lower()
    return (nlp(lowered), nltk.text.Text(nltk.word_tokenize(lowered)))

def words_for_pos(tags, pos):
    return [t for t in tags if t.pos_ == pos]

def concordance_dict_for_pos(tags, text, pos):
    return {t.lemma_:([c.line for c in text.concordance_list(t.norm_)]) for t in words_for_pos(tags, pos)}
