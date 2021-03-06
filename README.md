### predict film property
Predict MDHS (MD direction Heat shrinkage property of film) using sensored data.

### Introduction

In order to manufacture a PET film, first, a solid PET chip is melted, pressed in a DIE process, and then rapidly cooled to form an amorphous sheet. Thereafter, while heating the sheet, it is stretched in the longitudinal direction (TD) and the width direction (MD). The PET film manufactured in this way is for packaging, industrial, and optical purposes.
Material) and solar power.

Heat shrinkage is a physical property value that refers to the rate at which heat is applied to a product, and varies depending on stretching, heat treatment, and extrusion conditions. 18’
SKC Data CoE Pjt selected products produced by SKC's 7th unit as a target. In the case of No. 7, the film is stretched in the width direction (MD).
The process (TSM) has a characteristic consisting of a total of 13 Tenters. In this Tenter, not only the transverse direction (MD) is stretched, but also heat is simultaneously
Apply.
Among the various factors that determine film heat shrinkage, this analysis includes crystallinity (CHIP properties: IV), draw ratio (MD/TD direction stretching & relaxation),
A model was developed to predict the heat contraction rate through factors such as stretching speed and heat treatment (stretch zone, heat stabilization, etc).

### Modeling
The model for the MD direction heat shrinkage was estimated through the factors based on knowledge in the field. The estimated model was developed as a prediction trend chart screen and a simulation service screen. Both services are due to the needs of the field, and for this, the model was proposed as Regression. Regression may be somewhat insufficient in predictive power than other machine learning models, but there are parts that are easy to interpret. Due to the nature of a conservative factory, a model that can be understood well in the field, such as regression, is more appropriate than the Black-Box model(And it is also a good model to judge the degree of agreement between the knowledge of the field and the analyzed model). In addition, regression is also a way to show good predictive power as long as the relationship between actual factors is well discovered and applied to data well. Therefore, I decided that there was no big problem.

### Result
The predictive power was evaluated quantitatively and qualitatively. Quantitative evaluation was performed through MAPE and RMSE, and qualitative evaluation was performed through graphs.

 + Quantitative evaluation is below.

![coefficient](https://github.com/arloe/predict_film_property/blob/main/img/The%20coefficients.PNG)

 + Qualitative evaluation is below.
 
![graph](https://github.com/arloe/predict_film_property/blob/main/img/The%20graph.PNG)

### Remark
For more details, please refer to the pdf file.

### Reference
 + Jung Gyu Lee. (2010). Investigation of Properties of the PET Film Dependent on the Biaxial Stretching, Polymer (Korea) v.34 no.6 , 2010, pp.579 - 587  
 + Cribari-Neto F. (2004). Asymptotic Inference under Heteroskedasticity of Unknown Form. Computational Statistics & Data Analysis 45, 215–233