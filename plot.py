import pandas as pd
import matplotlib.pyplot as plt

# ranking learning types based on student's average success rates
df = pd.read_csv('difficulty_features.csv')

df.dropna(subset=['avg_success_rate', 'lrn_type'], inplace=True)

learning_type_categories = {
    'association': 'Association',
    'choicematrix': 'Choice Matrix',
    'clozeassociation': 'Cloze Association',
    'formulaV2': 'Formula Version 2',
    'imageclozeassociation': 'Image Cloze Association',
    'mcq': 'Multiple Choice Questions',
    'plaintext': 'Plain Text',
    'shorttext': 'Short Text',
    'sortlist': 'Sort List'
}

df['lrn_type'] = df['lrn_type'].map(learning_type_categories)

avg_success_by_type = df.groupby('lrn_type')['avg_success_rate'].mean()

sorted_learning_types = avg_success_by_type.sort_values(ascending=True).index.tolist()

plt.figure(figsize=(10, 6))
colors = plt.cm.get_cmap('tab10', len(sorted_learning_types))

for rank, lrn_type in enumerate(sorted_learning_types, start=1):
    subset = df[df['lrn_type'] == lrn_type]
    mean_rate = avg_success_by_type[lrn_type]
    plt.scatter([mean_rate] * len(subset), subset['lrn_type'], color=colors(rank-1), label=f"{len(sorted_learning_types)-rank+1}: {lrn_type}")


plt.yticks(ticks=range(1, len(sorted_learning_types)+1), labels=[""]*len(sorted_learning_types))

plt.xlabel('Average Success Rate')
plt.ylabel('Learning Types')
plt.title('Learning Type Rankings by Success Rate')
handles, labels = plt.gca().get_legend_handles_labels()
plt.legend(handles[::-1], labels[::-1], title='Learning Types Ranked by Success Rate', loc='center left', bbox_to_anchor=(1, 0.5))
plt.tight_layout()
plt.show()
