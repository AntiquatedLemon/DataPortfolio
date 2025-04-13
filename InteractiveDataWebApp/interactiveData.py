#all requirements
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import altair as alt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.preprocessing import StandardScaler

#title
st.header("Interactive Data Web App")

#gimme data to ingest!!!
uploaded_file = st.file_uploader("Upload a CSV file", type=["csv"])

# If a file is uploaded, process it
if uploaded_file is not None:
    # Load data into a pandas dataframe
    df = pd.read_csv(uploaded_file)
    
    # Display the first few rows of the dataframe
    st.header("Dataset Preview:", divider="gray")
    st.dataframe(df.head(10))

    # Show basic information about the dataframe
    st.header("Basic Information:", divider = "gray")
    st.write(f"Shape: {df.shape[0]} by {df.shape[1]}")
    st.write(f"Column : Data Type")
    st.write(df.dtypes.astype(str).to_dict())

    st.write(f"Missing Values:")
    # Render only the columns that are missing values and how many
    nullseries = df.isnull().sum()
    st.write(nullseries[nullseries>0])
    
    # Show statistical summary
    st.write("Statistical Summary:")
    st.write(df.describe())
    
    # Visualize data using seaborn
    st.subheader("Data Visualization")
    
    # Plot a pairplot if there are less than 15 columns
    st.subheader("Pairplot")
    if df.shape[1] <= 15:
        fig = sns.pairplot(df)
        st.pyplot(fig) # future proof this later
    else:
        st.write("Pairplot skipped due to too many columns.")
        # Maybe let the user choose which pairplots they want to see?
  
    # Check variable type
    numType = df.select_dtypes(include = ['int64', 'float64']).columns
    objType = df.select_dtypes(include = ['object']).columns

    # Single Variable Views
    st.markdown("## Single Variable")
    st.subheader("Choose Column to Visualize")
    interestVariable = st.selectbox("Select", df.columns)

    graphType = st.selectbox("Boxplot or Histogram?", ("Boxplot", "Histogram"), index = None, placeholder="Graph type...")
    st.write("Selection:", graphType)

    # One quantitative: boxplot
    if graphType == "Boxplot" and interestVariable in numType:
        fig, ax = plt.subplots()
        ax.boxplot(df[interestVariable], vert = False)

        ax.set_title(f"{graphType} of {interestVariable}")
        ax.set_xlabel(f"{interestVariable}")
        ax.set_ylabel("Value")

        st.pyplot(fig)
    # One quantitative:  histogram
    elif graphType == "Histogram" and interestVariable in numType: 
        fig, ax = plt.subplots()
        # bins not working, fix immediately
        ax.hist(df[interestVariable], bins = 10)

        ax.set_title(f"{graphType} of {interestVariable}")
        ax.set_xlabel("Value")
        ax.set_ylabel("Frequency")

        st.pyplot(fig)
    else:
        st.write("Perhaps you want more than one variable to look at? Continue!")

    # Multivariable Views
    st.markdown("## Two Variables")
    # Option to select columns for visualizations
    st.subheader("Choose Columns to Visualize")
    column_x = st.selectbox("Select Feature for X-axis", df.columns)
    column_y = st.selectbox("Select Feature for Y-axis", df.columns)
    
    # One Quantitative v Categorical: bar graph
    if (column_x in numType and column_y in objType) or (column_x in objType and column_y in numType):
       st.title(f"{column_x} by {column_y}")
       st.bar_chart(df, x = f"{column_x}", y = f"{column_y}")
    
    # grouped bar graph, segmented/stacked (y is sales, x is platform, xsub is genre)
       #st.selectbox("Segment further", df.columns)
       fig, ax = plt.subplots()
       chart = alt.Chart(df).mark_bar().encode(
            x = alt.X('Platform:N', title = 'Platform'),
            y = alt.Y('Global_Sales:Q', title = 'Sales'),
            color = 'Genre',
            column = 'Genre'
        ).properties(
            title = "Bar plot confused"
        )
       chart.show()

    # mosaic graph
    if column_x in objType and column_y in objType:
        st.title(f"??? graph of x on y")  
    else:
        st.write("graph broke")
    
    


    # Two quantitative: Parallel Boxplot and Scatterplot
    if column_x in numType and column_y in numType:
        st.subheader(f"Parallel Boxplots of {column_x} and {column_y}")
        fig, ax = plt.subplots()
        dataList = [df[column_x], df[column_y]]
        ax.boxplot(dataList, vert = False)
        ax.set_title(f"Boxplot of {column_x} and {column_y}")
        ax.set_ylabel("Value")
        st.pyplot(fig)
        
        st.write(f"Scatter Plot of {column_x} vs {column_y}")
        fig, ax = plt.subplots()     
        ax.scatter(df[column_x], df[column_y])
        ax.set_xlabel(column_x)
        ax.set_ylabel(column_y)
        st.pyplot(fig)
    
    
    # machine learning access here

else:
    st.write("Please provide data!")

# please note that the AM flag does not show on chromium browsers
st.write("Thank you for exploring with me :flag-us::smile::hearts:")