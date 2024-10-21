import numpy
import surveyequivalence
from surveyequivalence import PluralityVote, AgreementScore, AnalysisPipeline
import pandas as pd

survey = pd.read_csv('survey_results.csv')

plurality_combiner = PluralityVote(allowable_labels=['vid1', 'vid2'])
agreement_score = AgreementScore()
pipeline = AnalysisPipeline(survey,
                            classifier_predictions= survey.prediction,
                            combiner=plurality_combiner,
                            scorer=agreement_score,
                            allowable_labels=['vid1', 'vid2'],
                            verbosity = 1)