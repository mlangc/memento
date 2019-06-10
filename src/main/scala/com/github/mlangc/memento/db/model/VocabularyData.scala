package com.github.mlangc.memento.db.model

case class VocabularyData(language1: LanguageName,
                          language2: LanguageName,
                          translations: List[Translation],
                          synonyms1: List[Synonym],
                          synonyms2: List[Synonym],
                          checks: List[Check])
